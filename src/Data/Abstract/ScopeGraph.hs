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
  , newScope
  , associatedScope
  , insertDeclarationScope
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
  , declarations :: Map Declaration (Span, Maybe scopeAddress)
  } deriving (Eq, Show, Ord)


data ScopeGraph scope = ScopeGraph { graph :: Map scope (Scope scope), currentScope :: Maybe scope }

emptyGraph :: Ord scope => ScopeGraph scope
emptyGraph = ScopeGraph mempty Nothing

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
pathsOfScope scope = fmap references . Map.lookup scope . graph

ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Declaration (Span, Maybe scope))
ddataOfScope scope = fmap declarations . Map.lookup scope . graph

linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . graph

lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . graph

declare :: Ord scope => Declaration -> Span -> Maybe scope -> ScopeGraph scope -> ScopeGraph scope
declare declaration ddata assocScope g@ScopeGraph{..} = fromMaybe g $ do
  scopeKey <- currentScope
  scope <- lookupScope scopeKey g
  let newScope = scope { declarations = Map.insert declaration (ddata, assocScope) (declarations scope) }
  pure $ g { graph = (Map.insert scopeKey newScope graph) }

reference :: Ord scope => Reference -> Declaration -> ScopeGraph scope -> ScopeGraph scope
reference ref declaration g@ScopeGraph{..} = fromMaybe g $ do
  currentAddress <- currentScope
  currentScope' <- lookupScope currentAddress g
  go currentAddress currentScope' currentAddress id
  where
    declDataOfScope address = do
      dataMap <- ddataOfScope address g
      Map.lookup declaration dataMap
    go currentAddress currentScope address path =
      case declDataOfScope address of
          Just ddata ->
            let newScope = currentScope { references = Map.insert ref (path (DPath declaration)) (references currentScope) }
            in Just (g { graph = Map.insert currentAddress newScope graph })
          Nothing -> let
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              -- Return the first path to the declaration through the scopes.
              getFirst (foldMap (First . ap (go currentAddress currentScope) ((path .) . EPath edge)) scopes)
            in traverseEdges I <|> traverseEdges P

insertDeclarationScope :: Ord address => Declaration -> address -> ScopeGraph address -> ScopeGraph address
insertDeclarationScope decl address g@ScopeGraph{..} = fromMaybe g $ do
  declScope <- scopeOfDeclaration decl g
  scope <- lookupScope declScope g
  (span, _) <- Map.lookup decl (declarations scope)
  pure $ g { graph = Map.insert declScope (scope { declarations = Map.insert decl (span, Just address) (declarations scope) }) graph }

newScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newScope address edges g@ScopeGraph{..} = g { graph = Map.insert address newScope graph }
  where
    newScope = Scope edges mempty mempty

scopeOfRef :: Ord scope => Reference -> ScopeGraph scope -> Maybe scope
scopeOfRef ref g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      pathMap <- pathsOfScope s g
      _ <- Map.lookup ref pathMap
      pure (Just s)
    go [] = Nothing

pathOfRef :: (Ord scope) => Reference -> ScopeGraph scope -> Maybe (Path scope)
pathOfRef ref graph = do
  scope <- scopeOfRef ref graph
  pathsMap <- pathsOfScope scope graph
  Map.lookup ref pathsMap

-- Returns the scope the declaration was declared in.
scopeOfDeclaration :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
scopeOfDeclaration declaration g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      ddataMap <- ddataOfScope s g
      _ <- Map.lookup declaration ddataMap
      pure (Just s)
    go [] = Nothing

associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope declaration g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      ddataMap <- ddataOfScope s g
      (_, assocScope) <- Map.lookup declaration ddataMap
      pure assocScope
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
