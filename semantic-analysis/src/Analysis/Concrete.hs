{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, NamedFieldPuns,
             OverloadedStrings, QuantifiedConstraints, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators,
             UndecidableInstances #-}
module Analysis.Concrete
( Concrete(..)
, concrete
, heapGraph
, heapValueGraph
, heapAddressGraph
, addressStyle
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Export.Dot as G
import qualified Analysis.Carrier.Env.Precise as A
import qualified Analysis.Carrier.Heap.Precise as A
import           Analysis.Effect.Domain
import           Analysis.File
import qualified Analysis.Intro as I
import           Analysis.Name
import           Control.Algebra
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader hiding (Local)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Function (fix)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Semigroup (Last (..))
import           Data.Text (Text, pack)
import           Data.Traversable (for)
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Scope
import qualified System.Path as Path

type Addr = Int
type Env = Map.Map Name Addr

data Concrete term
  = Closure Path.AbsRelFile Span Name (Scope () term Addr)
  | Unit
  | Bool Bool
  | String Text
  | Record Env
  -- NB: We derive the 'Semigroup' instance for 'Concrete' to take the second argument. This is equivalent to stating that the return value of an imperative sequence of statements is the value of its final statement.
  deriving Semigroup via Last (Concrete term)

deriving instance ( forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Concrete f)
deriving instance ( forall a . Eq   a => Eq   (f a)
                  , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Concrete f)
deriving instance ( forall a . Show a => Show (f a))          => Show (Concrete f)


-- recordFrame :: Concrete term -> Maybe Env
-- recordFrame (Record frame) = Just frame
-- recordFrame _              = Nothing

type Heap = IntMap.IntMap


concrete
  :: Applicative term
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m (Concrete term))
     -> (term Addr -> m (Concrete term))
     )
  -> [File (term Addr)]
  -> (Heap (Concrete term), [File (Either (Path.AbsRelFile, Span, String) (Concrete term))])
concrete eval
  = run
  . evalFresh 0
  . A.runHeap
  . traverse (runFile eval)

runFile
  :: forall term m sig
  .  ( Applicative term
     , Effect sig
     , Has Fresh sig m
     , Has (A.Heap Addr (Concrete term)) sig m
     )
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m (Concrete term))
     -> (term Addr -> m (Concrete term))
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) (Concrete term)))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runFail
            . runReader @Env mempty
            . A.runEnv
            . fix (\ eval' -> runDomain eval' . fix eval)

-- concreteAnalysis
--   :: forall term m sig
--   .  ( Has (A.Env Addr) sig m
--      , Has (A.Heap Addr (Concrete term)) sig m
--      , Has (State (Heap (Concrete term))) sig m
--      )
--   => Analysis Addr (Concrete term) m
-- concreteAnalysis = Analysis{..}
--   where -- abstract _ name body = do
--         --   path <- ask
--         --   span <- ask
--         --   env <- asks (flip Map.restrictKeys (Set.delete name (foldMap Set.singleton body)))
--         --   pure (Closure path span name body env)
--         -- apply eval (Closure path span name body env) a = do
--         --   local (const path) . local (const span) $ do
--         --     addr <- A.alloc name
--         --     A.assign addr a
--         --     local (const (Map.insert name addr env)) (eval body)
--         -- apply _ f _ = fail $ "Cannot coerce " <> show f <> " to function"
--         record fields = do
--           fields' <- for fields $ \ (name, value) -> do
--             addr <- A.alloc name
--             A.assign addr value
--             pure (name, addr)
--           pure (Record (Map.fromList fields'))
--         addr ... n = do
--           val <- A.deref @Addr @(Concrete term) addr
--           heap <- get
--           pure (val >>= lookupConcrete heap n)


-- lookupConcrete :: Heap (Concrete term) -> Name -> Concrete (term Addr) -> Maybe Addr
-- lookupConcrete heap name = run . evalState IntSet.empty . runNonDetA . inConcrete
--   where -- look up the name in a concrete value
--         inConcrete = inFrame <=< maybeA . recordFrame
--         -- look up the name in a specific 'Frame', with slots taking precedence over parents
--         inFrame fs = maybeA (Map.lookup name fs) <|> (maybeA (Map.lookup "__semantic_super" fs) >>= inAddress)
--         -- look up the name in the value an address points to, if we haven’t already visited it
--         inAddress addr = do
--           visited <- get
--           guard (addr `IntSet.notMember` visited)
--           -- FIXME: throw an error if we can’t deref @addr@
--           val <- maybeA (IntMap.lookup addr heap)
--           modify (IntSet.insert addr)
--           inConcrete val
--         maybeA = maybe empty pure

runDomain :: (term Addr -> m (Concrete term)) -> DomainC term m a -> m a
runDomain eval (DomainC m) = runReader eval m

newtype DomainC term m a = DomainC (ReaderC (term Addr -> m (Concrete term)) m a)
  deriving (Applicative, Functor, Monad, MonadFail)

instance MonadTrans (DomainC term) where
  lift = DomainC . lift

instance ( Applicative term
         , Has (A.Env Addr) sig m
         , Has (A.Heap Addr (Concrete term)) sig m
         , Has (Reader Path.AbsRelFile) sig m
         , Has (Reader Span) sig m
         )
      => Algebra (Domain term Addr (Concrete term) :+: sig) (DomainC term m) where
  alg = \case
    L (Abstract i k) -> case i of
      I.Unit            -> k Unit
      I.Bool b          -> k (Bool b)
      I.String s        -> k (String s)
      I.Lam (Named n b) -> do
        path <- ask
        span <- ask
        k (Closure path span n b)
      I.Record fields   -> do
        eval <- DomainC ask
        fields' <- for fields $ \ (name, t) -> do
          addr <- A.alloc name
          v <- lift (eval t)
          A.assign @Addr @(Concrete term) addr v
          pure (name, addr)
        k (Record (Map.fromList fields'))
    L (Concretize c k) -> case c of
      Unit            -> k I.Unit
      Bool b          -> k (I.Bool b)
      String s        -> k (I.String s)
      Closure _ _ n b -> k (I.Lam (Named n b))
      Record fields   -> k (I.Record (map (fmap pure) (Map.toList fields)))
    R other -> DomainC (send (handleCoercible other))


-- | 'heapGraph', 'heapValueGraph', and 'heapAddressGraph' allow us to conveniently export SVGs of the heap:
--
--   > λ let (heap, res) = concrete [ruby]
--   > λ writeFile "/Users/rob/Desktop/heap.dot" (export (addressStyle heap) (heapAddressGraph heap))
--   > λ :!dot -Tsvg < ~/Desktop/heap.dot > ~/Desktop/heap.svg
heapGraph :: Foldable term => (Addr -> Concrete term -> a) -> (Either Edge Name -> Addr -> G.Graph a) -> Heap (Concrete term) -> G.Graph a
heapGraph vertex edge h = foldr (uncurry graph) G.empty (IntMap.toList h)
  where graph k v rest = (G.vertex (vertex k v) `G.connect` outgoing v) `G.overlay` rest
        outgoing = \case
          Unit -> G.empty
          Bool _ -> G.empty
          String _ -> G.empty
          Closure _ _ _ b -> foldr (G.overlay . edge (Left Lexical)) G.empty b
          Record frame -> Map.foldrWithKey (\ k -> G.overlay . edge (Right k)) G.empty frame

heapValueGraph :: Foldable term => Heap (Concrete term) -> G.Graph (Concrete term)
heapValueGraph h = heapGraph (const id) (const fromAddr) h
  where fromAddr addr = maybe G.empty G.vertex (IntMap.lookup addr h)

heapAddressGraph :: Foldable term => Heap (Concrete term) -> G.Graph (EdgeType (Concrete term), Addr)
heapAddressGraph = heapGraph (\ addr v -> (Value v, addr)) (fmap G.vertex . (,) . either Edge Slot)

addressStyle :: Heap (Concrete term) -> G.Style (EdgeType (Concrete term), Addr) Text
addressStyle heap = (G.defaultStyle vertex) { G.edgeAttributes }
  where vertex (_, addr) = pack (show addr) <> " = " <> maybe "?" fromConcrete (IntMap.lookup addr heap)
        edgeAttributes _ (Slot name,    _) = ["label" G.:= unName name]
        edgeAttributes _ (Edge Import,  _) = ["color" G.:= "blue"]
        edgeAttributes _ (Edge Lexical, _) = ["color" G.:= "green"]
        edgeAttributes _ _                 = []
        fromConcrete = \case
          Unit ->  "()"
          Bool b -> pack $ show b
          String s -> pack $ show s
          Closure p (Span s e) n _ -> "\\\\ " <> unName n <> " [" <> pack (Path.toString p) <> ":" <> showPos s <> "-" <> showPos e <> "]"
          Record _ -> "{}"
        showPos (Pos l c) = pack (show l) <> ":" <> pack (show c)

data EdgeType value
  = Edge Edge
  | Slot Name
  | Value value
  deriving (Eq, Ord, Show)

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)
