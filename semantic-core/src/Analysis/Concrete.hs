{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, TypeOperators, UndecidableInstances #-}
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
import           Data.Monoid (Alt(..))
import           Data.Name
import           Prelude hiding (fail)

type Precise = Int
type Env = Map.Map Name Precise

newtype FrameId = FrameId { unFrameId :: Precise }
  deriving (Eq, Ord, Show)

data Concrete
  = Closure Loc Name Core.Core Precise
  | Unit
  | Bool Bool
  | String String
  | Obj Frame
  deriving (Eq, Ord, Show)

objectFrame :: Concrete -> Maybe Frame
objectFrame (Obj frame) = Just frame
objectFrame _           = Nothing

data Frame = Frame
  { frameEdges :: [(Core.Edge, Precise)]
  , frameSlots :: Env
  }
  deriving (Eq, Ord, Show)

type Heap = IntMap.IntMap Concrete


-- | Concrete evaluation of a term to a value.
--
--   >>> snd (concrete [File (Loc "bool" emptySpan) (Core.Bool True)])
--   [Right (Bool True)]
concrete :: [File Core.Core] -> (Heap, [File (Either (Loc, String) Concrete)])
concrete
  = run
  . runFresh
  . runHeap
  . traverse runFile

runFile :: ( Carrier sig m
           , Effect sig
           , Member Fresh sig
           , Member (Reader FrameId) sig
           , Member (State Heap) sig
           )
        => File Core.Core
        -> m (File (Either (Loc, String) Concrete))
runFile file = traverse run file
  where run = runReader (fileLoc file)
            . runFailWithLoc
            . fix (eval concreteAnalysis)

concreteAnalysis :: ( Carrier sig m
                    , Member Fresh sig
                    , Member (Reader Loc) sig
                    , Member (Reader FrameId) sig
                    , Member (State Heap) sig
                    , MonadFail m
                    )
                 => Analysis Precise Concrete m
concreteAnalysis = Analysis{..}
  where alloc _ = fresh
        bind name addr = modifyCurrentFrame (updateFrameSlots (Map.insert name addr))
        lookupEnv n = do
          FrameId frameAddr <- ask
          val <- deref frameAddr
          heap <- get
          pure (val >>= lookupConcrete heap n)
        deref = gets . IntMap.lookup
        assign addr value = modify (IntMap.insert addr value)
        abstract _ name body = do
          loc <- ask
          FrameId parentAddr <- ask
          pure (Closure loc name body parentAddr)
        apply eval (Closure loc name body parentAddr) a = do
          frameAddr <- fresh
          assign frameAddr (Obj (Frame [(Core.Lexical, parentAddr)] mempty))
          local (const loc) . (frameAddr ...) $ do
            addr <- alloc name
            assign addr a
            bind name addr
            eval body
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
        frame = do
          lexical <- asks unFrameId
          pure (Obj (Frame [(Core.Lexical, lexical)] mempty))
        -- FIXME: throw an error
        -- FIXME: support dynamic imports
        edge e addr = modifyCurrentFrame (\ (Frame ps fs) -> Frame ((e, addr) : ps) fs)
        addr ... m = local (const (FrameId addr)) m

        updateFrameSlots f frame = frame { frameSlots = f (frameSlots frame) }

        modifyCurrentFrame f = do
          addr <- asks unFrameId
          Just (Obj frame) <- deref addr
          assign addr (Obj (f frame))


lookupConcrete :: Heap -> Name -> Concrete -> Maybe Precise
lookupConcrete heap name = run . evalState IntSet.empty . runNonDet . inConcrete
  where -- look up the name in a concrete value
        inConcrete = inFrame <=< maybeA . objectFrame
        -- look up the name in a specific 'Frame', with slots taking precedence over parents
        inFrame (Frame ps fs) = maybeA (Map.lookup name fs) <|> getAlt (foldMap (Alt . inAddress . snd) ps)
        -- look up the name in the value an address points to, if we haven’t already visited it
        inAddress addr = do
          visited <- get
          guard (addr `IntSet.notMember` visited)
          -- FIXME: throw an error if we can’t deref @addr@
          val <- maybeA (IntMap.lookup addr heap)
          modify (IntSet.insert addr)
          inConcrete val
        maybeA = maybe empty pure


runHeap :: (Carrier sig m, Member Fresh sig) => ReaderC FrameId (StateC Heap m) a -> m (Heap, a)
runHeap m = do
  addr <- fresh
  runState (IntMap.singleton addr (Obj (Frame [] mempty))) (runReader (FrameId addr) m)


-- | 'heapGraph', 'heapValueGraph', and 'heapAddressGraph' allow us to conveniently export SVGs of the heap:
--
--   > λ let (heap, res) = concrete [ruby]
--   > λ writeFile "/Users/rob/Desktop/heap.dot" (export (addressStyle heap) (heapAddressGraph heap))
--   > λ :!dot -Tsvg < ~/Desktop/heap.dot > ~/Desktop/heap.svg
heapGraph :: (Precise -> Concrete -> a) -> (Either Core.Edge Name -> Precise -> G.Graph a) -> Heap -> G.Graph a
heapGraph vertex edge h = foldr (uncurry graph) G.empty (IntMap.toList h)
  where graph k v rest = (G.vertex (vertex k v) `G.connect` outgoing v) `G.overlay` rest
        outgoing = \case
          Unit -> G.empty
          Bool _ -> G.empty
          String _ -> G.empty
          Closure _ _ _ parentAddr -> edge (Left Core.Lexical) parentAddr
          Obj frame -> fromFrame frame
        fromFrame (Frame es ss) = foldr (G.overlay . uncurry (edge . Left)) (foldr (G.overlay . uncurry (edge . Right)) G.empty (Map.toList ss)) es

heapValueGraph :: Heap -> G.Graph Concrete
heapValueGraph h = heapGraph (const id) (const fromAddr) h
  where fromAddr addr = maybe G.empty G.vertex (IntMap.lookup addr h)

heapAddressGraph :: Heap -> G.Graph (EdgeType, Precise)
heapAddressGraph = heapGraph (\ addr v -> (Value v, addr)) (fmap G.vertex . (,) . either Edge Slot)

addressStyle :: Heap -> G.Style (EdgeType, Precise) String
addressStyle heap = (G.defaultStyle vertex) { G.edgeAttributes }
  where vertex (_, addr) = maybe (show addr <> " = ?") (((show addr <> " = ") <>) . fromConcrete) (IntMap.lookup addr heap)
        edgeAttributes _ (Slot name,         _) = ["label" G.:= fromName name]
        edgeAttributes _ (Edge Core.Import,  _) = ["color" G.:= "blue"]
        edgeAttributes _ (Edge Core.Lexical, _) = ["color" G.:= "green"]
        edgeAttributes _ _                      = []
        fromConcrete = \case
          Unit ->  "()"
          Bool b -> show b
          String s -> show s
          Closure (Loc p (Span s e)) n _ _ -> "\\\\ " <> fromName n <> " [" <> p <> ":" <> showPos s <> "-" <> showPos e <> "]"
          Obj _ -> "{}"
        showPos (Pos l c) = show l <> ":" <> show c
        fromName (User s)  = s
        fromName (Gen sym) = fromGensym sym
        fromName (Path p)  = show p
        fromGensym (Root s) = s
        fromGensym (ss :/ (s, i)) = fromGensym ss <> "." <> s <> show i

data EdgeType
  = Edge Core.Edge
  | Slot Name
  | Value Concrete
  deriving (Eq, Ord, Show)
