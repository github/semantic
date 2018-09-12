{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
module Data.Abstract.ScopeGraph
  ( ScopeGraph(..)
  , Path
  , Reference(..)
  , Declaration(..)
  , EdgeLabel(..)
  , Heap
  , frameLookup
  , scopeLookup
  , frameSlots
  , frameLinks
  , getSlot
  , setSlot
  , lookup
  , scopeOfRef
  , declare
  , emptyGraph
  , reference
  , create
  , currentScope
  ) where

import           Data.Abstract.Live
import           Data.Abstract.Name
import qualified Data.Map.Strict as Map
import           Data.Semigroup.Reducer
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

data Scope scopeAddress = Scope {
    edges        :: Map EdgeLabel [scopeAddress] -- Maybe Map EdgeLabel [Path scope]?
  , references   :: Map Reference (Path scopeAddress)
  , declarations :: Map Declaration Span
  } deriving (Eq, Show, Ord)


data ScopeGraph scope = ScopeGraph { unScopeGraph :: (Map scope (Scope scope), scope) }

emptyGraph :: scope -> ScopeGraph scope
emptyGraph scope = ScopeGraph (Map.singleton scope (Scope mempty mempty mempty), scope)

deriving instance Eq address => Eq (ScopeGraph address)
deriving instance Show address => Show (ScopeGraph address)
deriving instance Ord address => Ord (ScopeGraph address)

data Path scope where
  DPath :: Declaration -> Path scope
  EPath :: EdgeLabel -> scope -> Path scope -> Path scope

deriving instance Eq scope => Eq (Path scope)
deriving instance Show scope => Show (Path scope)
deriving instance Ord scope => Ord (Path scope)

pathDeclaration :: Path scope -> Declaration
pathDeclaration (DPath d)     = d
pathDeclaration (EPath _ _ p) = pathDeclaration p

pathsOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Reference (Path scope))
pathsOfScope scope = fmap references . Map.lookup scope . fst . unScopeGraph

ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Declaration Span)
ddataOfScope scope = fmap declarations . Map.lookup scope . fst . unScopeGraph

linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . fst . unScopeGraph

lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . fst . unScopeGraph

currentScope :: ScopeGraph scope -> scope
currentScope = snd . unScopeGraph

scopeGraph :: ScopeGraph scope -> Map scope (Scope scope)
scopeGraph = fst . unScopeGraph

declare :: Ord scope => Declaration -> Span -> ScopeGraph scope -> ScopeGraph scope
declare declaration ddata graph = let scopeKey = currentScope graph
  in case lookupScope scopeKey graph of
    Just scope -> let newScope = scope { declarations = Map.insert declaration ddata (declarations scope) }
      in graph { unScopeGraph = (Map.insert scopeKey newScope (fst $ unScopeGraph graph), scopeKey) }
    Nothing -> graph

reference :: Ord scope => Reference -> Declaration -> ScopeGraph scope -> ScopeGraph scope
reference ref declaration graph = let
  currentAddress = currentScope graph
  declDataOfScope address = do
    dataMap <- ddataOfScope address graph
    Map.lookup declaration dataMap
  go currentScope address path =
    case declDataOfScope address of
        Just ddata ->
          let newScope = currentScope { references = Map.insert ref (path (DPath declaration)) (references currentScope) }
          in Just (graph { unScopeGraph = (Map.insert currentAddress newScope (scopeGraph graph), currentAddress) })
        Nothing -> let
          traverseEdges edge = do
            linkMap <- linksOfScope address graph
            scopes <- Map.lookup edge linkMap
            -- Return the first path to the declaration through the scopes.
            getFirst (foldMap (First . ap (go currentScope) ((path .) . EPath edge)) scopes)
          in traverseEdges P <|> traverseEdges I
  in case lookupScope currentAddress graph of
      Just currentScope -> fromMaybe graph (go currentScope currentAddress id)
      Nothing           -> graph

create :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
create address edges graph = graph { unScopeGraph = (Map.insert address newScope (scopeGraph graph), address) }
  where
    newScope = Scope edges mempty mempty

scopeOfRef :: Ord scope => Reference -> ScopeGraph scope -> Maybe scope
scopeOfRef ref graph = go . Map.keys . fst $ unScopeGraph graph
  where
    go (s : scopes') = case pathsOfScope s graph of
      Just pathMap -> case Map.lookup ref pathMap of
        Just _  -> Just s
        Nothing -> go scopes'
      Nothing -> go scopes'
    go [] = Nothing

pathOfRef :: (Ord scope) => Reference -> ScopeGraph scope -> Maybe (Path scope)
pathOfRef ref graph = do
  scope <- scopeOfRef ref graph
  pathsMap <- pathsOfScope scope graph
  Map.lookup ref pathsMap

scopeOfDeclaration :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
scopeOfDeclaration declaration graph = go . Map.keys . fst $ unScopeGraph graph
  where
    go (s : scopes') = case ddataOfScope s graph of
      Just ddataMap -> case Map.lookup declaration ddataMap of
        Just _  -> Just s
        Nothing -> go scopes'
      Nothing -> go scopes'
    go [] = Nothing

newtype Reference = Reference Name
  deriving (Eq, Ord, Show)

newtype Declaration = Declaration Name
  deriving (Eq, Ord, Show)

data EdgeLabel = P | I
  deriving (Eq, Ord, Show)

data Frame scopeAddress frameAddress declaration value = Frame {
    scopeAddress :: scopeAddress
  , links        :: Map EdgeLabel (Map scopeAddress frameAddress)
  , slots        :: Map declaration value
  }

newtype Heap scopeAddress frameAddress declaration value = Heap { unHeap :: Map frameAddress (Frame scopeAddress frameAddress declaration value) }

-- | Look up the frame for an 'address' in a 'Heap', if any.
frameLookup :: Ord address => address -> Heap scope address declaration value -> Maybe (Frame scope address declaration value)
frameLookup address = Map.lookup address . unHeap

-- | Look up the scope address for a given frame address.
scopeLookup :: Ord address => address -> Heap scope address declaration value -> Maybe scope
scopeLookup address = fmap scopeAddress . frameLookup address

frameSlots :: Ord address => address -> Heap scope address declaration value -> Maybe (Map declaration value)
frameSlots address = fmap slots . frameLookup address

frameLinks :: Ord address => address -> Heap scope address declaration value -> Maybe (Map EdgeLabel (Map scope address))
frameLinks address = fmap links . frameLookup address

getSlot :: (Ord address, Ord declaration) => address -> Heap scope address declaration value -> declaration -> Maybe value
getSlot address heap declaration = do
  slotMap <- frameSlots address heap
  Map.lookup declaration slotMap

setSlot :: (Ord address, Ord declaration) => address -> declaration -> value -> Heap scope address declaration value -> Heap scope address declaration value
setSlot address declaration value heap =
    case frameLookup address heap of
      Just frame -> let slotMap = slots frame in
        Heap $ Map.insert address (frame { slots = Map.insert declaration value slotMap }) (unHeap heap)
      Nothing -> heap

lookup :: (Ord address, Ord scope) => Heap scope address declaration value -> address -> Path scope -> declaration -> Maybe scope
lookup heap address (DPath d) declaration = scopeLookup address heap
lookup heap address (EPath label scope path) declaration = do
    frame <- frameLookup address heap
    scopeMap <- Map.lookup label (links frame)
    nextAddress <- Map.lookup scope scopeMap
    lookup heap nextAddress path declaration

newFrame :: (Ord address, Ord declaration) => scope -> address -> Map EdgeLabel (Map scope address) -> Heap scope address declaration value -> Heap scope address declaration value
newFrame scope address links = insertFrame address (Frame scope links mempty)

initFrame :: (Ord address, Ord declaration) => scope -> address -> Map EdgeLabel (Map scope address) -> Map declaration value -> Heap scope address declaration value -> Heap scope address declaration value
initFrame scope address links slots = fillFrame address slots . newFrame scope address links

insertFrame :: Ord address => address -> Frame scope address declaration value -> Heap scope address declaration value -> Heap scope address declaration value
insertFrame address frame = Heap . Map.insert address frame . unHeap

fillFrame :: Ord address => address -> Map declaration value -> Heap scope address declaration value -> Heap scope address declaration value
fillFrame address slots heap =
  case frameLookup address heap of
    Just frame -> insertFrame address (frame { slots = slots }) heap
    Nothing    -> heap

deleteFrame :: Ord address => address -> Heap scope address declaration value -> Heap scope address declaration value
deleteFrame address = Heap . Map.delete address . unHeap

-- | The number of frames in the `Heap`.
heapSize :: Heap scope address declaration value -> Int
heapSize = Map.size . unHeap

-- -- | Look up the list of values stored for a given address, if any.
-- scopeLookupAll :: Ord address => address -> Heap address value -> Maybe [value]
-- scopeLookupAll address = fmap toList . scopeLookup address

-- -- | Append a value onto the cell for a given address, inserting a new cell if none existed.
-- scopeInsert :: (Ord address, Ord value) => address -> value -> Scope address value -> Scope address value
-- scopeInsert address value = flip snoc (address, value)

-- -- | Manually insert a cell into the scope at a given address.
-- scopeInit :: Ord address => address -> Set value -> Scope address value -> Scope address value
-- scopeInit address cell (Scope h) = Scope (Map.insert address cell h)

-- -- | The number of addresses extant in a 'Scope'.
-- scopeSize :: Scope address value -> Int
-- scopeSize = Map.size . unScope

-- -- | Restrict a 'Scope' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
-- scopeRestrict :: Ord address => Scope address value -> Live address -> Scope address value
-- scopeRestrict (Scope m) roots = Scope (Map.filterWithKey (\ address _ -> address `liveMember` roots) m)

-- scopeDelete :: Ord address => address -> Scope address value -> Scope address value
-- scopeDelete addr = Scope . Map.delete addr . unScope

-- instance (Ord address, Ord value) => Reducer (address, value) (Scope address value) where
--   unit = Scope . unit
--   cons (addr, a) (Scope scope) = Scope (cons (addr, a) scope)
--   snoc (Scope scope) (addr, a) = Scope (snoc scope (addr, a))

-- instance (Show address, Show value) => Show (Scope address value) where
--   showsPrec d = showsUnaryWith showsPrec "Scope" d . map (second toList) . Map.pairs . unScope
