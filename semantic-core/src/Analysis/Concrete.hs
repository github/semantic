{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, RecordWildCards, TypeOperators, UndecidableInstances #-}
module Analysis.Concrete
( Concrete(..)
, concrete
, concreteAnalysis
, heapGraph
, heapValueGraph
, heapAddressGraph
, addressStyle
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Export.Dot as G
import           Analysis.Eval
import           Control.Applicative (Alternative (..))
import           Control.Effect
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.NonDet
import           Control.Effect.Reader hiding (Local)
import           Control.Effect.State
import           Control.Monad ((<=<), guard)
import qualified Data.Core as Core
import           Data.File
import           Data.Function (fix)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import qualified Data.Set as Set
import           Data.Term
import           Data.Text (Text, pack)
import           Data.Traversable (for)
import           Prelude hiding (fail)

type Precise = Int
type Env = Map.Map User Precise

newtype FrameId = FrameId { unFrameId :: Precise }
  deriving (Eq, Ord, Show)

data Concrete
  = Closure Loc User (Term Core.Core User) Env
  | Unit
  | Bool Bool
  | String Text
  | Obj Env
  deriving (Eq, Ord, Show)

objectFrame :: Concrete -> Maybe Env
objectFrame (Obj frame) = Just frame
objectFrame _           = Nothing

newtype Frame = Frame
  { frameSlots :: Env
  }
  deriving (Eq, Ord, Show)

type Heap = IntMap.IntMap Concrete


-- | Concrete evaluation of a term to a value.
--
--   >>> map fileBody (snd (concrete [File (Loc "bool" emptySpan) (Core.bool True)]))
--   [Right (Bool True)]
concrete :: [File (Term Core.Core User)] -> (Heap, [File (Either (Loc, String) Concrete)])
concrete
  = run
  . runFresh
  . runHeap
  . traverse runFile

runFile :: ( Carrier sig m
           , Effect sig
           , Member Fresh sig
           , Member (State Heap) sig
           )
        => File (Term Core.Core User)
        -> m (File (Either (Loc, String) Concrete))
runFile file = traverse run file
  where run = runReader (fileLoc file)
            . runFailWithLoc
            . runReader (mempty :: Env)
            . fix (eval concreteAnalysis)

concreteAnalysis :: ( Carrier sig m
                    , Member Fresh sig
                    , Member (Reader Env) sig
                    , Member (Reader Loc) sig
                    , Member (State Heap) sig
                    , MonadFail m
                    )
                 => Analysis Precise Concrete m
concreteAnalysis = Analysis{..}
  where alloc _ = fresh
        bind name addr m = local (Map.insert name addr) m
        lookupEnv n = asks (Map.lookup n)
        deref = gets . IntMap.lookup
        assign addr value = modify (IntMap.insert addr value)
        abstract _ name body = do
          loc <- ask
          env <- asks (flip Map.restrictKeys (Set.delete name (foldMap Set.singleton body)))
          pure (Closure loc name body env)
        apply eval (Closure loc name body env) a = do
          local (const loc) $ do
            addr <- alloc name
            assign addr a
            local (const (Map.insert name addr env)) (eval body)
        apply _ f _ = fail $ "Cannot coerce " <> show f <> " to function"
        unit = pure Unit
        bool b = pure (Bool b)
        asBool (Bool b) = pure b
        asBool v        = fail $ "Cannot coerce " <> show v <> " to Bool"
        string s = pure (String s)
        asString (String s) = pure s
        asString v          = fail $ "Cannot coerce " <> show v <> " to String"
        -- FIXME: differential inheritance (reference fields instead of copying)
        -- FIXME: copy non-lexical parents deeply?
        record fields = do
          fields' <- for fields $ \ (name, value) -> do
            addr <- alloc name
            assign addr value
            pure (name, addr)
          pure (Obj (Map.fromList fields'))
        addr ... n = do
          val <- deref addr
          heap <- get
          pure (val >>= lookupConcrete heap n)


lookupConcrete :: Heap -> User -> Concrete -> Maybe Precise
lookupConcrete heap name = run . evalState IntSet.empty . runNonDet . inConcrete
  where -- look up the name in a concrete value
        inConcrete = inFrame <=< maybeA . objectFrame
        -- look up the name in a specific 'Frame', with slots taking precedence over parents
        inFrame fs = maybeA (Map.lookup name fs) <|> (maybeA (Map.lookup "__semantic_super" fs) >>= inAddress)
        -- look up the name in the value an address points to, if we haven’t already visited it
        inAddress addr = do
          visited <- get
          guard (addr `IntSet.notMember` visited)
          -- FIXME: throw an error if we can’t deref @addr@
          val <- maybeA (IntMap.lookup addr heap)
          modify (IntSet.insert addr)
          inConcrete val
        maybeA = maybe empty pure


runHeap :: StateC Heap m a -> m (Heap, a)
runHeap = runState mempty


-- | 'heapGraph', 'heapValueGraph', and 'heapAddressGraph' allow us to conveniently export SVGs of the heap:
--
--   > λ let (heap, res) = concrete [ruby]
--   > λ writeFile "/Users/rob/Desktop/heap.dot" (export (addressStyle heap) (heapAddressGraph heap))
--   > λ :!dot -Tsvg < ~/Desktop/heap.dot > ~/Desktop/heap.svg
heapGraph :: (Precise -> Concrete -> a) -> (Either Core.Edge User -> Precise -> G.Graph a) -> Heap -> G.Graph a
heapGraph vertex edge h = foldr (uncurry graph) G.empty (IntMap.toList h)
  where graph k v rest = (G.vertex (vertex k v) `G.connect` outgoing v) `G.overlay` rest
        outgoing = \case
          Unit -> G.empty
          Bool _ -> G.empty
          String _ -> G.empty
          Closure _ _ _ env -> foldr (G.overlay . edge (Left Core.Lexical)) G.empty env
          Obj frame -> foldr (G.overlay . uncurry (edge . Right)) G.empty (Map.toList frame)

heapValueGraph :: Heap -> G.Graph Concrete
heapValueGraph h = heapGraph (const id) (const fromAddr) h
  where fromAddr addr = maybe G.empty G.vertex (IntMap.lookup addr h)

heapAddressGraph :: Heap -> G.Graph (EdgeType, Precise)
heapAddressGraph = heapGraph (\ addr v -> (Value v, addr)) (fmap G.vertex . (,) . either Edge Slot)

addressStyle :: Heap -> G.Style (EdgeType, Precise) Text
addressStyle heap = (G.defaultStyle vertex) { G.edgeAttributes }
  where vertex (_, addr) = pack (show addr) <> " = " <> maybe "?" fromConcrete (IntMap.lookup addr heap)
        edgeAttributes _ (Slot name,         _) = ["label" G.:= name]
        edgeAttributes _ (Edge Core.Import,  _) = ["color" G.:= "blue"]
        edgeAttributes _ (Edge Core.Lexical, _) = ["color" G.:= "green"]
        edgeAttributes _ _                      = []
        fromConcrete = \case
          Unit ->  "()"
          Bool b -> pack $ show b
          String s -> pack $ show s
          Closure (Loc p (Span s e)) n _ _ -> "\\\\ " <> n <> " [" <> p <> ":" <> showPos s <> "-" <> showPos e <> "]"
          Obj _ -> "{}"
        showPos (Pos l c) = pack (show l) <> ":" <> pack (show c)

data EdgeType
  = Edge Core.Edge
  | Slot User
  | Value Concrete
  deriving (Eq, Ord, Show)


-- $setup
-- >>> :seti -XOverloadedStrings
