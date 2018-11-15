{-# LANGUAGE GADTs, DeriveAnyClass, DuplicateRecordFields, TupleSections #-}
module Data.Abstract.ScopeGraph
  ( Address(..)
  , associatedScope
  , Declaration(..) -- TODO don't export these constructors
  , declare
  , EdgeLabel(..)
  , insertDeclarationScope
  , insertImportReference
  , newScope
  , insertScope
  , insertEdge
  , Path(..)
  , pathDeclaration
  , pathOfRef
  , pathPosition
  , Position(..)
  , reference
  , Reference(..) -- TODO don't export these constructors
  , ScopeGraph(..)
  , lookupScope
  , lookupScopePath
  , Scope(..)
  , scopeOfRef
  , pathDeclarationScope
  , putDeclarationScopeAtPosition
  ) where

import Control.Abstract.Hole
import           Data.Abstract.Name
import qualified Data.Map.Strict as Map
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue
import qualified Data.Sequence as Seq

data Address address = Address { frameAddress :: address, position :: Position }
    deriving (Eq, Show, Ord, Generic, NFData)

-- Offsets and frame addresses in the heap should be addresses?
data Scope scopeAddress = Scope {
    edges        :: Map EdgeLabel [scopeAddress] -- Maybe Map EdgeLabel [Path scope]?
  , references   :: Map Reference (Path scopeAddress)
  , declarations :: Seq (Declaration, (Span, Maybe scopeAddress))
  } deriving (Eq, Show, Ord, Generic, NFData)

instance Lower (Scope scopeAddress) where
  lowerBound = Scope mempty mempty mempty

instance AbstractHole (Scope scopeAddress) where
  hole = lowerBound

instance AbstractHole address => AbstractHole (Address address) where
  hole = Address hole (Position 0)

newtype Position = Position { unPosition :: Int }
  deriving (Eq, Show, Ord, Generic, NFData)

data ScopeGraph scope = ScopeGraph { graph :: Map scope (Scope scope), currentScope :: Maybe scope }

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty Nothing

deriving instance Eq address => Eq (ScopeGraph address)
deriving instance Show address => Show (ScopeGraph address)
deriving instance Ord address => Ord (ScopeGraph address)
deriving instance Generic (ScopeGraph address)
deriving instance NFData scope => NFData (ScopeGraph scope)

data Path scope where
  -- | Construct a direct path to a declaration.
  Hole :: Path scope
  DPath :: Declaration -> Position -> Path scope
  -- | Construct an edge from a scope to another declaration path.
  EPath :: EdgeLabel -> scope -> Path scope -> Path scope

instance AbstractHole (Path scope) where
  hole = Hole

deriving instance Eq scope => Eq (Path scope)
deriving instance Show scope => Show (Path scope)
deriving instance Ord scope => Ord (Path scope)
deriving instance Generic (Path scope)
deriving instance NFData scope => NFData (Path scope)
deriving instance Functor Path

-- Returns the declaration of a path.
pathDeclaration :: Path scope -> Declaration
pathDeclaration (DPath d _)   = d
pathDeclaration (EPath _ _ p) = pathDeclaration p
pathDeclaration Hole          = undefined

-- TODO: Store the current scope closer _in_ the DPath?
pathDeclarationScope :: Maybe scope -> Path scope -> Maybe scope
pathDeclarationScope _ (EPath _ scope (DPath d _)) = Just scope
pathDeclarationScope currentScope (EPath _ _ p) = pathDeclarationScope currentScope p
pathDeclarationScope currentScope (DPath d _) = currentScope

-- TODO: Possibly return in Maybe since we can have Hole paths
pathPosition :: Path scope -> Position
pathPosition Hole    = Position 0
pathPosition (DPath _ p)   = p
pathPosition (EPath _ _ p) = pathPosition p

-- Returns the reference paths of a scope in a scope graph.
pathsOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Reference (Path scope))
pathsOfScope scope = fmap references . Map.lookup scope . graph

-- Returns the declaration data of a scope in a scope graph.
ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Seq (Declaration, (Span, Maybe scope)))
ddataOfScope scope = fmap declarations . Map.lookup scope . graph

-- Returns the edges of a scope in a scope graph.
linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . graph

-- Lookup a scope in the scope graph.
lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . graph

-- Declare a declaration with a span and an associated scope in the scope graph.
-- TODO: Return the whole value in Maybe or Either.
declare :: Ord scope => Declaration -> Span -> Maybe scope -> ScopeGraph scope -> (ScopeGraph scope, Maybe Position)
declare declaration ddata assocScope g@ScopeGraph{..} = fromMaybe (g, Nothing) $ do
  scopeKey <- currentScope
  scope <- lookupScope scopeKey g
  let newScope = scope { declarations = (declarations scope) Seq.|> (declaration, (ddata, assocScope)) }
  pure $ (g { graph = Map.insert scopeKey newScope graph }, Just . Position $ length (declarations newScope))

-- | Add a reference to a declaration in the scope graph.
-- Returns the original scope graph if the declaration could not be found.
reference :: Ord scope => Reference -> Declaration -> ScopeGraph scope -> ScopeGraph scope
reference ref decl@Declaration{..} g@ScopeGraph{..} = fromMaybe g $ do
  -- Start from the current address
  currentAddress <- currentScope
  currentScope' <- lookupScope currentAddress g
  -- Build a path up to the declaration
  go currentAddress currentScope' currentAddress id
  where
    go currentAddress currentScope address path =
      case lookupDeclaration unDeclaration address g of
          Just (_, index) ->
            let newScope = currentScope { references = Map.insert ref (path (DPath decl index)) (references currentScope) }
            in Just (g { graph = Map.insert currentAddress newScope graph })
          Nothing -> let
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              -- Return the first path to the declaration through the scopes.
              getFirst (foldMap (First . ap (go currentAddress currentScope) ((path .) . EPath edge)) scopes)
            in traverseEdges Import <|> traverseEdges Lexical

-- | Insert a reference into the given scope by constructing a resolution path to the declaration within the given scope graph.
insertImportReference :: Ord address => Reference -> Declaration -> ScopeGraph address -> address -> Scope address -> Maybe (Scope address)
insertImportReference ref decl@Declaration{..} g@ScopeGraph{..} scopeAddress scope = do
  currentAddress <- currentScope
  go currentAddress (EPath Import currentAddress)
  where
    go address path =
      case lookupDeclaration unDeclaration address g of
        Just (_, index) ->
          Just $ scope { references = Map.insert ref (path (DPath decl index)) (references scope) }
        Nothing -> traverseEdges Import <|> traverseEdges Lexical
          where
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              -- Return the first path to the declaration through the scopes.
              getFirst (foldMap (First . (\scope -> go scope (path . EPath edge scope))) scopes)

lookupScopePath :: (Ord scopeAddress, Show scopeAddress) => Name -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupScopePath declaration g@ScopeGraph{..} = do
  currentAddress <- currentScope
  go currentAddress id
  where
    go address path =
      case lookupDeclaration declaration address g of
        Just (_, index) -> Just $ path (DPath (Declaration declaration) index)
        Nothing -> maybe Nothing (Just . path) (lookupReference declaration address g)
          <|> traverseEdges Import <|> traverseEdges Lexical
          where
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              getFirst (foldMap (First . (\scope -> go scope (path . EPath edge scope))) scopes)

lookupDeclaration :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe ((Declaration, (Span, Maybe scopeAddress)), Position)
lookupDeclaration declaration scope g = do
  dataSeq <- ddataOfScope scope g
  index <- Seq.findIndexR (((Declaration declaration) ==) . fst) dataSeq
  (, Position index) <$> Seq.lookup index dataSeq

putDeclarationScopeAtPosition :: Ord scopeAddress => scopeAddress -> Position -> Maybe scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
putDeclarationScopeAtPosition scope position assocScope g = fromMaybe g $ do
  dataSeq <- ddataOfScope scope g
  let seq = Seq.adjust' (\(d, (span, _)) -> (d, (span, assocScope))) (unPosition position) dataSeq
  pure $ g { graph = Map.adjust (\s -> s { declarations = seq }) scope (graph g) }

lookupReference :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupReference  name scope g = Map.lookup (Reference name) =<< pathsOfScope scope g

insertEdge :: Ord scopeAddress => EdgeLabel -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertEdge label target g@ScopeGraph{..} = fromMaybe g $ do
  scopeAddress <- currentScope
  currentScope' <- lookupScope scopeAddress g
  scopes <- maybe (Just mempty) pure (Map.lookup label (edges currentScope'))
  let newScope = currentScope' { edges = Map.insert label (target : scopes) (edges currentScope') }
  pure (g { graph = Map.insert scopeAddress newScope graph })


-- | Insert associate the given address to a declaration in the scope graph.
insertDeclarationScope :: Ord scopeAddress => Declaration -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertDeclarationScope decl@Declaration{..} address g@ScopeGraph{..} = fromMaybe g $ do
  declScope <- scopeOfDeclaration decl g
  (span, position) <- (fst . snd . fst &&& unPosition . snd) <$> lookupDeclaration unDeclaration declScope g
  scope <- lookupScope declScope g
  pure $ g { graph = Map.insert declScope (scope { declarations = Seq.adjust (const (decl, (span, Just address))) position (declarations scope) }) graph }

-- | Insert a new scope with the given address and edges into the scope graph.
newScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newScope address edges = insertScope address (Scope edges mempty mempty)

insertScope :: Ord address => address -> Scope address -> ScopeGraph address -> ScopeGraph address
insertScope address scope g@ScopeGraph{..} = g { graph = Map.insert address scope graph }

-- | Returns the scope of a reference in the scope graph.
scopeOfRef :: Ord scope => Reference -> ScopeGraph scope -> Maybe scope
scopeOfRef ref g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      pathMap <- pathsOfScope s g
      _ <- Map.lookup ref pathMap
      pure (Just s)
    go [] = Nothing

-- | Returns the path of a reference in the scope graph.
pathOfRef :: (Ord scope) => Reference -> ScopeGraph scope -> Maybe (Path scope)
pathOfRef ref graph = do
  scope <- scopeOfRef ref graph
  pathsMap <- pathsOfScope scope graph
  Map.lookup ref pathsMap

-- Returns the scope the declaration was declared in.
scopeOfDeclaration :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
scopeOfDeclaration Declaration{..} g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (scope : scopes') = fromMaybe (go scopes') $ lookupDeclaration unDeclaration scope g >> pure (Just scope)
    go [] = Nothing

-- | Returns the scope associated with a declaration (the child scope if any exists).
associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope Declaration{..} g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (scope : scopes') = fromMaybe (go scopes') $ snd . snd . fst <$> lookupDeclaration unDeclaration scope g
    go [] = Nothing

newtype Reference = Reference { unReference :: Name }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype Declaration = Declaration { unDeclaration :: Name }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import | Export
  deriving (Eq, Ord, Show, Generic, NFData)
