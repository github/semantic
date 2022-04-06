{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Syntax
( Term(..)
  -- * Abstract interpretation
, eval0
, eval
, evalModule0
, evalModule
  -- * Macro-expressible syntax
, let'
, letrec
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
import           Analysis.Name (Name, name, nameI)
import           Analysis.Reference as Ref
import           Control.Applicative (Alternative (..), liftA3)
import           Control.Effect.Labelled
import           Control.Effect.Throw (Throw, throwError)
import           Control.Monad (guard)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import           Data.Function (fix)
import qualified Data.IntMap as IntMap
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty, fromList, toList)
import           Data.Monoid (First (..))
import qualified Data.Set as Set
import           Data.String (IsString (..))
import           Data.Text (Text, pack)
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
  | Function Name [Name] Term
  deriving (Eq, Ord, Show)


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has S.Statement sig m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has S.Statement sig m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  Var n     -> lookupEnv n >>= maybe (dvar n) fetch
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
  Import ns -> S.simport ns >> dunit
  Function n ps b -> letrec n (dabs ps (\ as ->
    foldr (uncurry let') (eval b) (zip ps as)))


evalModule0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m) => Term -> m (Module val)
evalModule0 i = S.runStatement mk (eval0 i) where
  mk msgs b = pure (Module (const b) (Set.fromList (map (\ (S.Import cs) -> name (Text.intercalate (pack ".") (toList cs))) msgs)) mempty mempty)

evalModule :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m) => (Term -> S.StatementC m val) -> (Term -> m (Module val))
evalModule f i = S.runStatement mk (eval f i) where
  mk msgs b = pure (Module (const b) (Set.fromList (map (\ (S.Import cs) -> name (Text.intercalate (pack ".") (toList cs))) msgs)) mempty mempty)


-- Macro-expressible syntax

let' :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> val -> m a -> m a
let' n v m = do
  addr <- alloc n
  addr .= v
  bind n addr m

letrec :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> m val -> m val
letrec n m = do
  addr <- alloc n
  v <- bind n addr m
  addr .= v
  pure v


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
  edges <- o A..: fromString "edges"
  index <- o A..: fromString "id"
  o A..: fromString "attrs" >>= A.withObject "attrs" (\ attrs -> do
    ty <- attrs A..: fromString "type"
    node <- parseType attrs edges ty
    pure (index, node, node <$ guard (ty == "module")))

parseType :: A.Object -> [A.Value] -> String -> A.Parser (IntMap.IntMap Term -> Term)
parseType attrs edges = \case
  "string"     -> const . String <$> attrs A..: fromString "text"
  "true"       -> pure (const (Bool True))
  "false"      -> pure (const (Bool False))
  "throw"      -> fmap Throw <$> resolve (head edges)
  "if"         -> liftA3 Iff <$> findEdgeNamed edges "condition" <*> findEdgeNamed edges "consequence" <*> findEdgeNamed edges "alternative" <|> pure (const Noop)
  "block"      -> children edges
  "module"     -> children edges
  "identifier" -> const . Var . name <$> attrs A..: fromString "text"
  "import"     -> const . Import . fromList . map snd . sortOn fst <$> traverse (resolveWith (const moduleNameComponent)) edges
  t            -> A.parseFail ("unrecognized type: " <> t <> " attrs: " <> show attrs <> " edges: " <> show edges)

findEdgeNamed :: (Foldable t, A.FromJSON a, Eq a) => t A.Value -> a -> A.Parser (IntMap.IntMap rep -> rep)
findEdgeNamed edges name = foldMap (resolveWith (\ rep attrs -> attrs A..: fromString "type" >>= (rep <$) . guard . (== name))) edges

-- | Map a list of edges to a list of child nodes.
children :: [A.Value] -> A.Parser (IntMap.IntMap Term -> Term)
children edges = fmap (foldr chain Noop . zip [0..]) . sequenceA <$> traverse resolve edges

moduleNameComponent :: A.Object -> A.Parser (Int, Text)
moduleNameComponent attrs = (,) <$> attrs A..: fromString "index" <*> attrs A..: fromString "text"

-- | Chain a statement before any following syntax by let-binding it. Note that this implies call-by-value since any side effects in the statement must be performed before the let's body.
chain :: (Int, Term) -> Term -> Term
chain = uncurry (Let . nameI)

resolve :: A.Value -> A.Parser (IntMap.IntMap rep -> rep)
resolve = resolveWith (const . pure)

resolveWith :: ((IntMap.IntMap rep -> rep) -> A.Object -> A.Parser a) -> A.Value -> A.Parser a
resolveWith f = A.withObject "edge" (\ edge -> do
  sink <- edge A..: fromString "sink"
  attrs <- edge A..: fromString "attrs"
  f (IntMap.! sink) attrs)
