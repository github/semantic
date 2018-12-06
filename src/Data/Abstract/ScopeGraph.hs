{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, GADTs, TupleSections #-}
module Data.Abstract.ScopeGraph
  ( Slot(..)
  , associatedScope
  , Declaration(..) -- TODO don't export these constructors
  , declare
  , EdgeLabel(..)
  , insertDeclarationScope
  , insertDeclarationSpan
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
  , declarationNames
  ) where

import           Control.Abstract.Hole
import           Data.Abstract.Name
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

-- A slot is a location in the heap where a value is stored.
data Slot address = Slot { frameAddress :: address, position :: Position }
    deriving (Eq, Show, Ord, Generic, NFData)

-- Offsets and frame addresses in the heap should be addresses?
data Scope scopeAddress = Scope
  { edges        :: Map EdgeLabel [scopeAddress] -- Maybe Map EdgeLabel [Path scope]?
  , references   :: Map Reference (Path scopeAddress)
  , declarations :: Seq (Declaration, (Span, Maybe scopeAddress))
  } deriving (Eq, Show, Ord, Generic, NFData)

instance Lower (Scope scopeAddress) where
  lowerBound = Scope mempty mempty mempty

instance AbstractHole (Scope scopeAddress) where
  hole = lowerBound

instance AbstractHole address => AbstractHole (Slot address) where
  hole = Slot hole (Position 0)

newtype Position = Position { unPosition :: Int }
  deriving (Eq, Show, Ord, Generic, NFData)

newtype ScopeGraph scope = ScopeGraph { unScopeGraph :: Map scope (Scope scope) }

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty

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
pathDeclarationScope :: scope -> Path scope -> Maybe scope
pathDeclarationScope _ (EPath _ scope (DPath _ _)) = Just scope
pathDeclarationScope currentScope (EPath _ _ p)    = pathDeclarationScope currentScope p
pathDeclarationScope currentScope (DPath _ _)      = Just currentScope
pathDeclarationScope _ Hole                        = Nothing

-- TODO: Possibly return in Maybe since we can have Hole paths
pathPosition :: Path scope -> Position
pathPosition Hole          = Position 0
pathPosition (DPath _ p)   = p
pathPosition (EPath _ _ p) = pathPosition p

-- Returns the reference paths of a scope in a scope graph.
pathsOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Reference (Path scope))
pathsOfScope scope = fmap references . Map.lookup scope . unScopeGraph

-- Returns the declaration data of a scope in a scope graph.
ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Seq (Declaration, (Span, Maybe scope)))
ddataOfScope scope = fmap declarations . Map.lookup scope . unScopeGraph

-- Returns the edges of a scope in a scope graph.
linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . unScopeGraph

-- Lookup a scope in the scope graph.
lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . unScopeGraph

-- Declare a declaration with a span and an associated scope in the scope graph.
-- TODO: Return the whole value in Maybe or Either.
declare :: Ord scope => Declaration -> Span -> Maybe scope -> scope -> ScopeGraph scope -> (ScopeGraph scope, Maybe Position)
declare declaration ddata assocScope currentScope g = fromMaybe (g, Nothing) $ do
  scope <- lookupScope currentScope g

  dataSeq <- ddataOfScope currentScope g
  case Seq.findIndexR (\(decl, (span, _)) -> decl == declaration && ddata == span) dataSeq of
    Just index -> pure (g, Just (Position index))
    Nothing -> do
      let newScope = scope { declarations = declarations scope Seq.|> (declaration, (ddata, assocScope)) }
      pure (insertScope currentScope newScope g, Just (Position (length (declarations newScope))))

-- | Add a reference to a declaration in the scope graph.
-- Returns the original scope graph if the declaration could not be found.
reference :: Ord scope => Reference -> Declaration -> scope -> ScopeGraph scope -> ScopeGraph scope
reference ref decl currentAddress g = fromMaybe g $ do
  -- Start from the current address
  currentScope' <- lookupScope currentAddress g
  -- Build a path up to the declaration
  go lowerBound currentScope' currentAddress id
  where
    go visited currentScope address path
      | address `Set.member` visited = Nothing
      | otherwise
        =   flip (insertScope currentAddress) g . flip (insertReference ref) currentScope . path <$> pathToDeclaration decl address g
        <|> traverseEdges' Superclass <|> traverseEdges' Import <|> traverseEdges' Export <|> traverseEdges' Lexical
      where traverseEdges' edge = linksOfScope address g >>= Map.lookup edge >>= traverseEdges path (go (Set.insert address visited) currentScope) edge

-- | Insert a reference into the given scope by constructing a resolution path to the declaration within the given scope graph.
insertImportReference :: Ord address => Reference -> Declaration -> address -> ScopeGraph address -> Scope address -> Maybe (Scope address)
insertImportReference ref decl currentAddress g scope = go lowerBound currentAddress (EPath Import currentAddress)
  where
    go visited address path
      | address `Set.member` visited = Nothing
      | otherwise
        =   flip (insertReference ref) scope . path <$> pathToDeclaration decl address g
        <|> traverseEdges' Superclass <|> traverseEdges' Import <|> traverseEdges' Export <|> traverseEdges' Lexical
      where traverseEdges' edge = linksOfScope address g >>= Map.lookup edge >>= traverseEdges path (go (Set.insert address visited)) edge

lookupScopePath :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupScopePath declaration currentAddress g = go lowerBound currentAddress id
  where
    go visited address path
      | address `Set.member` visited = Nothing
      | otherwise
        =   path <$> pathToDeclaration (Declaration declaration) address g
        <|> path <$> lookupReference declaration address g
        <|> traverseEdges' Superclass <|> traverseEdges' Import <|> traverseEdges' Export <|> traverseEdges' Lexical
      where traverseEdges' edge = linksOfScope address g >>= Map.lookup edge >>= traverseEdges path (go (Set.insert address visited)) edge

pathToDeclaration :: Ord scopeAddress => Declaration -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
pathToDeclaration decl address g = DPath decl . snd <$> lookupDeclaration (unDeclaration decl) address g

insertReference :: Reference -> Path scopeAddress -> Scope scopeAddress -> Scope scopeAddress
insertReference ref path scope = scope { references = Map.insert ref path (references scope) }

traverseEdges :: Foldable t => (Path scopeAddress -> Path scopeAddress) -> (scopeAddress -> (Path scopeAddress -> Path scopeAddress) -> Maybe a) -> EdgeLabel -> t scopeAddress -> Maybe a
-- Return the first path to the declaration through the scopes.
traverseEdges path go edge = getFirst . foldMap (First . (go <*> fmap path . EPath edge))

lookupDeclaration :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe ((Declaration, (Span, Maybe scopeAddress)), Position)
lookupDeclaration declaration scope g = do
  dataSeq <- ddataOfScope scope g
  index <- Seq.findIndexR ((Declaration declaration ==) . fst) dataSeq
  (, Position index) <$> Seq.lookup index dataSeq

declarationNames :: Ord address => [EdgeLabel] -> Scope address -> ScopeGraph address -> Set Declaration
declarationNames edgeLabels scope scopeGraph = localDeclarations <> edgeNames
  where addresses = join (Map.elems $ Map.restrictKeys (edges scope) (Set.fromList edgeLabels))
        edgeNames = flip foldMap addresses $ \address -> maybe mempty (flip (declarationNames edgeLabels) scopeGraph) (lookupScope address scopeGraph)
        localDeclarations = Set.fromList . toList . fmap fst $ declarations scope


putDeclarationScopeAtPosition :: Ord scopeAddress => scopeAddress -> Position -> Maybe scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
putDeclarationScopeAtPosition scope position assocScope g@(ScopeGraph graph) = fromMaybe g $ do
  dataSeq <- ddataOfScope scope g
  let seq = Seq.adjust' (\(d, (span, _)) -> (d, (span, assocScope))) (unPosition position) dataSeq
  pure $ ScopeGraph (Map.adjust (\s -> s { declarations = seq }) scope graph)

lookupReference :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupReference  name scope g = Map.lookup (Reference name) =<< pathsOfScope scope g

insertEdge :: Ord scopeAddress => EdgeLabel -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertEdge label target currentAddress g@(ScopeGraph graph) = fromMaybe g $ do
  currentScope' <- lookupScope currentAddress g
  scopes <- maybeM (Just mempty) (Map.lookup label (edges currentScope'))
  let newScope = currentScope' { edges = Map.insert label (target : scopes) (edges currentScope') }
  pure (ScopeGraph (Map.insert currentAddress newScope graph))


-- | Insert associate the given associated scope into the declaration in the scope graph.
insertDeclarationScope :: Ord scopeAddress => Declaration -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertDeclarationScope decl@Declaration{..} address currentAddress g@(ScopeGraph graph) = fromMaybe g $ do
  declScope <- pathDeclarationScope currentAddress =<< lookupScopePath unDeclaration currentAddress g
  (span, position) <- (fst . snd . fst &&& unPosition . snd) <$> lookupDeclaration unDeclaration declScope g
  scope <- lookupScope declScope g
  pure $ ScopeGraph (Map.insert declScope (scope { declarations = Seq.adjust (const (decl, (span, Just address))) position (declarations scope) }) graph)

-- | Insert a declaration span into the declaration in the scope graph.
insertDeclarationSpan :: Ord scopeAddress => Declaration -> Span -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertDeclarationSpan decl@Declaration{..} span g@(ScopeGraph graph) = fromMaybe g $ do
  declScope <- scopeOfDeclaration decl g
  (associatedScope, position) <- (snd . snd . fst &&& unPosition . snd) <$> lookupDeclaration unDeclaration declScope g
  scope <- lookupScope declScope g
  pure $ ScopeGraph (Map.insert declScope (scope { declarations = Seq.adjust (const (decl, (span, associatedScope))) position (declarations scope) }) graph)

-- | Insert a new scope with the given address and edges into the scope graph.
newScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newScope address edges = insertScope address (Scope edges mempty mempty)

insertScope :: Ord address => address -> Scope address -> ScopeGraph address -> ScopeGraph address
insertScope address scope = ScopeGraph . Map.insert address scope . unScopeGraph

-- | Returns the scope of a reference in the scope graph.
scopeOfRef :: Ord scope => Reference -> ScopeGraph scope -> Maybe scope
scopeOfRef ref g@(ScopeGraph graph) = go (Map.keys graph)
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
scopeOfDeclaration Declaration{..} g@(ScopeGraph graph) = go (Map.keys graph)
  where
    go = foldr (\ scope -> (scope <$ lookupDeclaration unDeclaration scope g <|>)) Nothing

-- | Returns the scope associated with a declaration (the child scope if any exists).
associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope Declaration{..} g@(ScopeGraph graph) = go (Map.keys graph)
  where
    go = foldr (\ scope -> ((lookupDeclaration unDeclaration scope g >>= snd . snd . fst) <|>)) Nothing

newtype Reference = Reference { unReference :: Name }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype Declaration = Declaration { unDeclaration :: Name }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import | Export | Superclass
  deriving (Eq, Ord, Show, Generic, NFData)
