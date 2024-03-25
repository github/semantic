{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- | This belongs in @semantic-python@ instead of @semantic-analysis@, but for the sake of expedience…
module Analysis.Syntax.Python
( -- * Syntax
  Term(..)
, subterms
  -- * Abstract interpretation
, eval0
, eval
  -- * Parsing
, parse
) where

import           Analysis.Effect.Domain hiding ((:>>>))
import qualified Analysis.Effect.Statement as S
import           Analysis.Name
import           Analysis.Reference
import           Analysis.VM
import           Control.Effect.Labelled
import           Control.Effect.Reader
import           Control.Monad (foldM)
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Language.Python.Common as Py
import           Language.Python.Version3.Parser
import           Source.Span (Pos (..), Span (..), point)
import           System.FilePath (takeBaseName)

-- Syntax

data Term
  = Module (Py.Module Py.SrcSpan)
  | Statement (Py.Statement Py.SrcSpan)
  | Expr (Py.Expr Py.SrcSpan)
  deriving (Eq, Ord, Show)

-- | Non-generic production of the recursive set of subterms.
--
-- This should be exactly the set of nodes which 'eval' can visit, i.e. it excludes comments, etc.
subterms :: Term -> Set.Set Term
subterms t = Set.insert t $ case t of
  Module (Py.Module ss)                 -> suite ss
  Statement (Py.Conditional cts e _)    -> foldMap (\ (c, t) -> subterms (Expr c) <> suite t) cts <> suite e
  Statement (Py.Raise (Py.RaiseV3 e) _) -> maybe Set.empty (subterms . Expr . fst) e
  -- FIXME: Py.RaiseV2
  -- FIXME: whatever the second field is
  Statement (Py.StmtExpr e _)           -> subterms (Expr  e)
  Statement (Py.Fun _ _ _ ss _)         -> suite ss
  -- FIXME: include 'subterms' of any default values
  Expr (Py.Call f as _)                 -> subterms (Expr f) <> foldMap (\case { Py.ArgExpr e _ -> subterms (Expr e) ; _ -> Set.empty }) as
  -- FIXME: support keyword args &c.
  _                                     -> Set.empty -- TBD, and terminals
  where
  suite = foldMap (subterms . Statement)


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m, MonadFail m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m, MonadFail m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  Module (Py.Module ss) -> suite ss
  Statement (Py.Import is sp) -> setSpan sp $ do
    for_ is $ \ Py.ImportItem{ Py.import_item_name = ns } -> case nonEmpty ns of
      Nothing -> pure ()
      Just ss -> S.simport (pack . Py.ident_string <$> ss)
    dunit
  Statement (Py.Pass sp) -> setSpan sp dunit
  Statement (Py.Conditional cts e sp) -> setSpan sp $ foldr (\ (c, t) e -> do
    c' <- eval (Expr c)
    dif c' (suite t) e) (suite e) cts
  Statement (Py.Raise (Py.RaiseV3 e) sp) -> setSpan sp $ case e of
    Just (e, _) -> eval (Expr e) >>= ddie -- FIXME: from clause
    Nothing     -> dunit >>= ddie
    -- FIXME: RaiseV2
    -- FIXME: whatever the second field is
  Statement (Py.StmtExpr e sp) -> setSpan sp (eval (Expr e))
  Statement (Py.Fun n ps _r ss sp) -> let ps' = mapMaybe (\case { Py.Param n _ _ _ -> Just (ident n) ; _ -> Nothing }) ps in setSpan sp $ letrec (ident n) (dabs ps' (foldr (\ (p, a) m -> let' p a m) (suite ss) . zip ps'))
  Expr (Py.Var n sp) -> setSpan sp $ let n' = ident n in lookupEnv n' >>= maybe (dvar n') fetch
  Expr (Py.Bool b sp) -> setSpan sp $ dbool b
  Expr (Py.Strings ss sp) -> setSpan sp $ dstring (pack (mconcat ss))
  Expr (Py.Call f as sp) -> setSpan sp $ do
    f' <- eval (Expr f)
    as' <- traverse eval (mapMaybe (\case { Py.ArgExpr e _ -> Just (Expr e) ; _ -> Nothing }) as)
    -- FIXME: support keyword args &c.
    dapp f' as'
  _ -> fail "TBD"
  where
  setSpan s = case fromSpan s of
    Just s -> local (\ r -> r{ refSpan = s })
    _      -> id
  fromSpan Py.SpanEmpty                     = Nothing
  fromSpan (Py.SpanPoint _ l c)             = Just (point (Pos l c))
  fromSpan (Py.SpanCoLinear _ l c1 c2)      = Just (Span (Pos l c1) (Pos l c2))
  fromSpan (Py.SpanMultiLine _ l1 l2 c1 c2) = Just (Span (Pos l1 c1) (Pos l2 c2))
  suite []     = dunit
  suite (s:ss) = do
    s' <- eval (Statement s)
    foldM (\ into each -> do
      each' <- eval (Statement each)
      into >>> each') s' ss
  ident = name . pack . Py.ident_string


-- Parsing

parse :: FilePath -> IO Term
parse path = do
  src <- readFile path
  case parseModule src (takeBaseName path) of
    Left err                -> fail (show err)
    Right (Py.Module ss, _) -> pure (Module (Py.Module ss))
