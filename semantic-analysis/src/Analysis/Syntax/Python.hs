{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- | This belongs in @semantic-python@ instead of @semantic-analysis@, but for the sake of expedience…
module Analysis.Syntax.Python
( -- * Syntax
  Term
, Python(..)
  -- * Abstract interpretation
, eval0
, eval
  -- * Parsing
, parse
) where

import           Analysis.Effect.Domain hiding ((:>>>), (>>>))
import qualified Analysis.Effect.Domain as D
import qualified Analysis.Effect.Statement as S
import           Analysis.Name
import           Analysis.Reference
import qualified Analysis.Syntax as T
import           Analysis.VM
import           Control.Effect.Labelled
import           Control.Effect.Reader
import           Data.Function (fix)
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import           Data.Text (Text, pack)
import qualified Language.Python.Common.AST as Py
import           Language.Python.Version3.Parser
import           Source.Span (Span)
import           System.FilePath (takeBaseName)

-- Syntax

type Term = T.Term Python Name

data Python t
  = Noop
  | Iff t t t
  | Bool Bool
  | String Text
  | Throw t
  | Let Name t t
  | t :>> t
  | Import (NonEmpty Text)
  | Function Name [Name] t
  | Call t [t]
  | Locate Span t
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 1 :>>


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  T.Var n  -> lookupEnv n >>= maybe (dvar n) fetch
  T.Term s -> case s of
    Noop      -> dunit
    Iff c t e -> do
      c' <- eval c
      dif c' (eval t) (eval e)
    Bool b    -> dbool b
    String s  -> dstring s
    Throw e   -> eval e >>= ddie
    Let n v b -> do
      v' <- eval v
      let' n v' (eval b)
    t :>> u   -> do
      t' <- eval t
      u' <- eval u
      t' D.>>> u'
    Import ns -> S.simport ns >> dunit
    Function n ps b -> letrec n (dabs ps (foldr (\ (p, a) m -> let' p a m) (eval b) . zip ps))
    Call f as -> do
      f' <- eval f
      as' <- traverse eval as
      dapp f' as'
    Locate s t -> local (setSpan s) (eval t)
  where
  setSpan s r = r{ refSpan = s }

(>>>) :: T.Term Python v -> T.Term Python v -> T.Term Python v
l >>> r = T.Term (l :>> r)

noop :: T.Term Python v
noop = T.Term Noop

iff :: T.Term Python v -> T.Term Python v -> T.Term Python v -> T.Term Python v
iff c t e = T.Term (Iff c t e)


-- Parsing

parse :: FilePath -> IO (T.Term Python Name)
parse path = do
  src <- readFile path
  case parseModule src (takeBaseName path) of
    Left err                -> fail (show err)
    Right (Py.Module ss, _) -> suite ss
  where
  statement :: Py.Statement annot -> IO (T.Term Python Name)
  statement = \case
    Py.Import is _ -> foldr ((>>>) . T.Term . Import) noop <$> traverse importItem is
    Py.Conditional cs e _ -> foldr (\ (c, t) e -> iff <$> expr c <*> suite t <*> e) (suite e) cs
    _ -> fail "cannot ingest this Python statement"
  expr :: Py.Expr annot -> IO (T.Term Python Name)
  expr = \case
    Py.Var v _ -> pure (T.Var (name (pack (ident v))))
    _ -> fail "cannot ingest this Python expression"
  ident :: Py.Ident annot -> String
  ident (Py.Ident s _) = s
  importItem :: Py.ImportItem annot -> IO (NonEmpty Text)
  importItem Py.ImportItem{ Py.import_item_name = ns } = maybe (fail "") pure (nonEmpty (map (pack . ident) ns)) -- FIXME: "as" names
  suite :: [Py.Statement annot] -> IO (T.Term Python Name)
  suite ss = foldr (>>>) noop <$> traverse statement ss
