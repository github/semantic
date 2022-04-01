{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Syntax
( Term(..)
, Syntax(..)
  -- * Pretty-printing
, Print(..)
  -- * Abstract interpretation
, eval0
, eval
, evalModule0
, evalModule
, Interpret(..)
  -- * Macro-expressible syntax
, let'
  -- * Parsing
, parseFile
, parseGraph
, parseNode
) where

import qualified Analysis.Carrier.Statement.State as S
import           Analysis.Effect.Domain
import           Analysis.Effect.Env (Env, bind, lookupEnv)
import           Analysis.Effect.Store
import           Analysis.File
import           Analysis.Module
import           Analysis.Name (Name, formatName, name, nameI)
import           Analysis.Reference as Ref
import           Control.Applicative (Alternative (..), liftA3)
import           Control.Effect.Labelled
import           Control.Effect.Throw (Throw, throwError)
import           Control.Monad (guard)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import           Data.Function (fix)
import qualified Data.IntMap as IntMap
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty, fromList, toList)
import           Data.Monoid (First (..))
import qualified Data.Set as Set
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified System.Path as Path

data Term
  = Var Name
  | Noop
  | Iff Term Term Term
  | Bool Bool
  | String Text
  | Throw Term
  | Let Name Term Term
  | Import (NonEmpty Text)
  deriving (Eq, Ord, Show)

-- TODO: 🔥
class Syntax rep where
  var :: Name -> rep

  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep

  let_ :: Name -> rep -> (rep -> rep) -> rep

  -- * Statements

  import' :: NonEmpty Text -> rep


-- Pretty-printing

newtype Print = Print { print_ :: ShowS }

instance Show Print where
  showsPrec _ = print_

instance Semigroup Print where
  Print a <> Print b = Print (a . b)

instance Monoid Print where
  mempty = Print id

instance Syntax Print where
  var n = str "get" <+> text (formatName n)

  iff c t e = parens (str "iff" <+> c <+> str "then" <+> t <+> str "else" <+> e)
  noop = parens (str "noop")

  bool b = parens (str (if b then "true" else "false"))
  string = parens . text

  throw e = parens (str "throw" <+> e)

  let_ n v b = let n' = text (formatName n) in parens (str "let" <+> n' <+> char '=' <+> v <+> str "in" <+> b n')

  import' ns = foldr1 (\ a b -> a <> text (pack ".") <> b) (text <$> ns)

str :: String -> Print
str = Print . showString

text :: Text -> Print
text = str . unpack

char :: Char -> Print
char = Print . showChar

parens :: Print -> Print
parens p = char '(' <> p <> char ')'

(<+>) :: Print -> Print -> Print
l <+> r = l <> char ' ' <> r

infixr 6 <+>


-- Abstract interpretation

eval0 :: Interpret m i -> m i
eval0 = fix eval

eval :: (Interpret m i -> m i) -> (Interpret m i -> m i)
eval eval (Interpret f) = f eval


evalModule0 :: Applicative m => Interpret (S.StatementC m) rep -> m (Module rep)
evalModule0 i = S.runStatement mk (eval0 i) where
  mk msgs b = pure (Module (const b) (Set.fromList (map (\ (S.Import cs) -> name (Text.intercalate (pack ".") (toList cs))) msgs)) mempty mempty)

evalModule :: Applicative m => (Interpret (S.StatementC m) rep -> (S.StatementC m) rep) -> (Interpret (S.StatementC m) rep -> m (Module rep))
evalModule f i = S.runStatement mk (eval f i) where
  mk msgs b = pure (Module (const b) (Set.fromList (map (\ (S.Import cs) -> name (Text.intercalate (pack ".") (toList cs))) msgs)) mempty mempty)


newtype Interpret m i = Interpret { interpret :: (Interpret m i -> m i) -> m i }

instance (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has S.Statement sig m) => Syntax (Interpret m val) where
  var n = Interpret (\ _ -> do
    a <- lookupEnv n
    maybe (dvar n) fetch a)

  iff c t e = Interpret (\ eval -> do
    c' <- eval c
    dif c' (eval t) (eval e))
  noop = Interpret (const dunit)

  bool b = Interpret (\ _ -> dbool b)
  string s = Interpret (\ _ -> dstring s)

  throw e = Interpret (\ eval -> eval e >>= ddie)

  let_ n v b = Interpret (\ eval -> do
    v' <- eval v
    let' n v' (eval (b (Interpret (pure (pure v'))))))

  import' ns = Interpret (\ _ -> do
    S.simport ns
    dunit)


-- Macro-expressible syntax

let' :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> val -> m a -> m a
let' n v m = do
  addr <- alloc n
  addr .= v
  bind n addr m


-- Parsing

parseFile :: (Has (Throw String) sig m, MonadIO m) => FilePath -> m (File Term)
parseFile path = do
  contents <- liftIO (B.readFile path)
  case (A.eitherDecodeWith A.json' (A.iparse parseGraph) contents) of
    Left  (_, err)       -> throwError err
    Right (_, Nothing)   -> throwError "no root node found"
    -- FIXME: this should get the path to the source file, not the path to the JSON.
    -- FIXME: this should use the span of the source file, not an empty span.
    Right (_, Just root) -> pure (File (Ref.fromPath (Path.absRel path)) root)

parseGraph :: A.Value -> A.Parser (IntMap.IntMap Term, Maybe Term)
parseGraph = A.withArray "nodes" $ \ nodes -> do
  (untied, First root) <- foldMap (\ (k, v, r) -> ([(k, v)], First r)) <$> traverse (A.withObject "node" parseNode) (V.toList nodes)
  -- @untied@ is a list of key/value pairs, where the keys are graph node IDs and the values are functions from the final graph to the representations of said graph nodes. Likewise, @root@ is a function of the same variety, wrapped in a @Maybe@.
  --
  -- We define @tied@ as the fixpoint of the former to yield the former as a graph of type @IntMap.IntMap Term@, and apply the latter to said graph to yield the entry point, if any, from which to evaluate.
  let tied = fix (\ tied -> ($ tied) <$> IntMap.fromList untied)
  pure (tied, ($ tied) <$> root)

parseNode :: A.Object -> A.Parser (IntMap.Key, IntMap.IntMap Term -> Term, Maybe (IntMap.IntMap Term -> Term))
parseNode o = do
  edges <- o A..: A.fromString "edges"
  index <- o A..: A.fromString "id"
  o A..: A.fromString "attrs" >>= A.withObject "attrs" (\ attrs -> do
    ty <- attrs A..: A.fromString "type"
    node <- parseType attrs edges ty
    pure (index, node, node <$ guard (ty == "module")))

parseType :: A.Object -> [A.Value] -> String -> A.Parser (IntMap.IntMap Term -> Term)
parseType attrs edges = \case
  "string"     -> const . String <$> attrs A..: A.fromString "text"
  "true"       -> pure (const (Bool True))
  "false"      -> pure (const (Bool False))
  "throw"      -> fmap Throw <$> resolve (head edges)
  "if"         -> liftA3 Iff <$> findEdgeNamed edges "condition" <*> findEdgeNamed edges "consequence" <*> findEdgeNamed edges "alternative" <|> pure (const Noop)
  "block"      -> children edges
  "module"     -> children edges
  "identifier" -> const . Var . name <$> attrs A..: A.fromString "text"
  "import"     -> const . Import . fromList . map snd . sortOn fst <$> traverse (resolveWith (const moduleNameComponent)) edges
  t            -> A.parseFail ("unrecognized type: " <> t <> " attrs: " <> show attrs <> " edges: " <> show edges)

findEdgeNamed :: (Foldable t, A.FromJSON a, Eq a) => t A.Value -> a -> A.Parser (IntMap.IntMap rep -> rep)
findEdgeNamed edges name = foldMap (resolveWith (\ rep attrs -> attrs A..: A.fromString "type" >>= (rep <$) . guard . (== name))) edges

-- | Map a list of edges to a list of child nodes.
children :: [A.Value] -> A.Parser (IntMap.IntMap Term -> Term)
children edges = fmap (foldr chain Noop . zip [0..]) . sequenceA <$> traverse resolve edges

moduleNameComponent :: A.Object -> A.Parser (Int, Text)
moduleNameComponent attrs = (,) <$> attrs A..: A.fromString "index" <*> attrs A..: A.fromString "text"

-- | Chain a statement before any following syntax by let-binding it. Note that this implies call-by-value since any side effects in the statement must be performed before the let's body.
chain :: (Int, Term) -> Term -> Term
chain = uncurry (Let . nameI)

resolve :: A.Value -> A.Parser (IntMap.IntMap rep -> rep)
resolve = resolveWith (const . pure)

resolveWith :: ((IntMap.IntMap rep -> rep) -> A.Object -> A.Parser a) -> A.Value -> A.Parser a
resolveWith f = A.withObject "edge" (\ edge -> do
  sink <- edge A..: A.fromString "sink"
  attrs <- edge A..: A.fromString "attrs"
  f (IntMap.! sink) attrs)
