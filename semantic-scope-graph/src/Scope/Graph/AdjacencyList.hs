{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Scope.Graph.AdjacencyList
  ( module Scope.Graph.AdjacencyList
  ) where

import           Analysis.Name
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Module
import           Data.Monoid
import           Data.Semilattice.Lower
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Scope.Info
import           Scope.Path
import           Scope.Reference
import           Scope.Scope
import           Scope.Types
import           Source.Span

newtype ScopeGraph scope = ScopeGraph { unScopeGraph :: Map scope (Scope scope) }
  deriving (Eq, Ord, Show)

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty

-- Returns the reference paths of a scope in a scope graph.
pathsOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Reference ([ReferenceInfo], Path scope))
pathsOfScope scope = fmap references . Map.lookup scope . unScopeGraph

-- Returns the declaration data of a scope in a scope graph.
ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Seq (Info scope))
ddataOfScope scope = fmap declarations . Map.lookup scope . unScopeGraph

-- Returns the edges of a scope in a scope graph.
linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . unScopeGraph

declarationsByAccessControl :: Ord scope => scope -> AccessControl -> ScopeGraph scope -> [ Info scope ]
declarationsByAccessControl scope accessControl g = fromMaybe mempty $ do
  dataSeq <- ddataOfScope scope g
  pure . toList $ Seq.filter (\Info{..} -> accessControl <= infoAccessControl) dataSeq

declarationsByRelation :: Ord scope => scope -> Relation -> ScopeGraph scope -> [ Info scope ]
declarationsByRelation scope relation g = fromMaybe mempty $ do
  dataSeq <- ddataOfScope scope g
  pure . toList $ Seq.filter (\Info{..} -> infoRelation == relation) dataSeq

declarationByName :: Ord scope => scope -> Declaration -> ScopeGraph scope -> Maybe (Info scope)
declarationByName scope name g = do
  dataSeq <- ddataOfScope scope g
  find (\Info{..} -> infoDeclaration == name) dataSeq

-- Lookup a scope in the scope graph.
lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . unScopeGraph

-- Declare a declaration with a span and an associated scope in the scope graph.
-- TODO: Return the whole value in Maybe or Either.
declare :: Ord scope
        => Declaration
        -> ModuleInfo
        -> Relation
        -> AccessControl
        -> Span
        -> Kind
        -> Maybe scope
        -> scope
        -> ScopeGraph scope
        -> (ScopeGraph scope, Maybe Position)
declare decl moduleInfo rel accessControl declSpan kind assocScope currentScope g = fromMaybe (g, Nothing) $ do
  scope <- lookupScope currentScope g
  dataSeq <- ddataOfScope currentScope g
  case Seq.findIndexR (\Info{..} -> decl == infoDeclaration && declSpan == infoSpan && rel == infoRelation) dataSeq of
    Just index -> pure (g, Just (Position index))
    Nothing -> do
      let newScope = scope { declarations = declarations scope Seq.|> Info decl moduleInfo rel accessControl declSpan kind assocScope }
      pure (insertScope currentScope newScope g, Just (Position (length (declarations newScope))))

-- | Add a reference to a declaration in the scope graph.
-- Returns the original scope graph if the declaration could not be found.
reference :: Ord scope => Reference -> ModuleInfo -> Span -> Kind -> Declaration -> scope -> ScopeGraph scope -> ScopeGraph scope
reference ref moduleInfo span kind decl currentAddress g = fromMaybe g $ do
  -- Start from the current address
  currentScope' <- lookupScope currentAddress g
  -- Build a path up to the declaration
  flip (insertScope currentAddress) g . flip (insertReference ref moduleInfo span kind) currentScope' <$> findPath (const Nothing) decl currentAddress g

-- | Insert a reference into the given scope by constructing a resolution path to the declaration within the given scope graph.
insertImportReference :: Ord address => Reference -> ModuleInfo -> Span -> Kind -> Declaration -> address -> ScopeGraph address -> Scope address -> Maybe (Scope address)
insertImportReference ref moduleInfo span kind decl currentAddress g scope = flip (insertReference ref moduleInfo span kind) scope . EPath Import currentAddress <$> findPath (const Nothing) decl currentAddress g

lookupScopePath :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupScopePath declaration currentAddress g = findPath (flip (lookupReference declaration) g) (Declaration declaration) currentAddress g

findPath :: Ord scopeAddress => (scopeAddress -> Maybe (Path scopeAddress)) -> Declaration -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
findPath extra decl currentAddress g = snd <$> getFirst (foldGraph combine currentAddress g)
  where combine address path = fmap (address, )
          $  First (pathToDeclaration decl address g)
          <> First (extra address)
          <> (uncurry (EPath Superclass) <$> path Superclass)
          <> (uncurry (EPath Import)     <$> path Import)
          <> (uncurry (EPath Export)     <$> path Export)
          <> (uncurry (EPath Lexical)    <$> path Lexical)

foldGraph :: (Ord scopeAddress, Monoid a) => (scopeAddress -> (EdgeLabel -> a) -> a) -> scopeAddress -> ScopeGraph scopeAddress -> a
foldGraph combine address graph = go lowerBound address
  where go visited address
          | address `Set.notMember` visited
          , Just edges <- linksOfScope address graph = combine address (recur edges)
          | otherwise                                = mempty
          where visited' = Set.insert address visited
                recur edges edge = maybe mempty (foldMap (go visited')) (Map.lookup edge edges)

pathToDeclaration :: Ord scopeAddress => Declaration -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
pathToDeclaration decl address g = DPath decl . snd <$> lookupDeclaration (unDeclaration decl) address g

insertReference :: Reference -> ModuleInfo -> Span -> Kind -> Path scopeAddress -> Scope scopeAddress -> Scope scopeAddress
insertReference ref moduleInfo span kind path scope = scope { references = Map.alter (\case
  Nothing -> pure ([ ReferenceInfo span kind moduleInfo ], path)
  Just (refInfos, path) -> pure (ReferenceInfo span kind moduleInfo : refInfos, path)) ref (references scope) }

-- | Adds a reference and a Hole path to the given scope.
newReference :: Reference -> ReferenceInfo -> Scope scopeAddress -> Scope scopeAddress
newReference ref info scope = scope { references = Map.alter (\case
  Nothing -> pure ([ info ], Hole)
  Just (refInfos, path) -> pure (info : refInfos, path)) ref (references scope) }

lookupDeclaration :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Info scopeAddress, Position)
lookupDeclaration name scope g = do
  dataSeq <- ddataOfScope scope g
  index <- Seq.findIndexR (\Info{..} -> Declaration name == infoDeclaration) dataSeq
  (, Position index) <$> Seq.lookup index dataSeq

declarationNames :: Ord address => [EdgeLabel] -> Scope address -> ScopeGraph address -> Set Declaration
declarationNames edgeLabels scope scopeGraph = localDeclarations <> edgeNames
  where addresses = join (Map.elems $ Map.restrictKeys (edges scope) (Set.fromList edgeLabels))
        edgeNames = flip foldMap addresses $ \address -> maybe mempty (flip (declarationNames edgeLabels) scopeGraph) (lookupScope address scopeGraph)
        localDeclarations = Set.fromList . toList . fmap infoDeclaration $ declarations scope


putDeclarationScopeAtPosition :: Ord scopeAddress => scopeAddress -> Position -> Maybe scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
putDeclarationScopeAtPosition scope position assocScope g@(ScopeGraph graph) = fromMaybe g $ do
  dataSeq <- ddataOfScope scope g
  let seq = Seq.adjust' (\Info{..} -> Info { infoAssociatedScope = assocScope, .. }) (unPosition position) dataSeq
  pure $ ScopeGraph (Map.adjust (\s -> s { declarations = seq }) scope graph)

-- | Lookup a reference by traversing the paths of a given scope and return a Maybe (Path address)
lookupReference :: Ord address => Name -> address -> ScopeGraph address -> Maybe (Path address)
lookupReference  name scope g = fmap snd . Map.lookup (Reference name) =<< pathsOfScope scope g

insertEdge :: Ord scopeAddress => EdgeLabel -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertEdge label target currentAddress g@(ScopeGraph graph) = fromMaybe g $ do
  currentScope' <- lookupScope currentAddress g
  scopes <- maybe (Just mempty) pure (Map.lookup label (edges currentScope'))
  let newScope = currentScope' { edges = Map.insert label (target : scopes) (edges currentScope') }
  pure (ScopeGraph (Map.insert currentAddress newScope graph))

insertEdges :: Ord scopeAddress => NonEmpty EdgeLabel -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertEdges labels target currentAddress g =
  foldr (\label graph -> insertEdge label target currentAddress graph) g labels

-- | Add an import edge of the form 'a -> Import -> b -> Import -> c' or creates intermediate void scopes of the form
--   'a -> Void -> b -> Import -> c' if the given scopes cannot be found.
addImportEdge :: Ord scopeAddress => EdgeLabel -> [scopeAddress] -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
addImportEdge edge importEdge currentAddress g = do
  case importEdge of
    [] -> g
    (name:[]) -> maybe
                (addImportHole edge name currentAddress g)
                (const (insertEdge edge name currentAddress g))
                (lookupScope name g)
    (name:names) -> let
      scopeGraph' = maybe
        (addImportHole edge name currentAddress g)
        (const (insertEdge edge name currentAddress g))
        (lookupScope name g)
      in
        addImportEdge edge names name scopeGraph'

addImportHole :: Ord scopeAddress => EdgeLabel -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
addImportHole edge name currentAddress g = let
  scopeGraph' = newScope name mempty g
  in
  insertEdges (NonEmpty.fromList [Void, edge]) name currentAddress scopeGraph'


-- | Update the 'Scope' containing a 'Declaration' with an associated scope address.
-- Returns an unmodified 'ScopeGraph' if the 'Declaration' cannot be found with the given scope address.
insertDeclarationScope :: Ord scopeAddress => Declaration -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertDeclarationScope Declaration{..} associatedScopeAddress scopeAddress g = fromMaybe g $ do
  declScopeAddress <- pathDeclarationScope scopeAddress =<< lookupScopePath unDeclaration scopeAddress g
  scope <- lookupScope declScopeAddress g
  (declInfo, position) <- second unPosition <$> lookupDeclaration unDeclaration declScopeAddress g
  pure $ insertScope declScopeAddress (scope { declarations = Seq.update position (declInfo { infoAssociatedScope = Just associatedScopeAddress }) (declarations scope) }) g

-- | Insert a declaration span into the declaration in the scope graph.
insertDeclarationSpan :: Ord scopeAddress => Declaration -> Span -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertDeclarationSpan decl@Declaration{..} span g = fromMaybe g $ do
  declScopeAddress <- scopeOfDeclaration decl g
  (declInfo, position) <- second unPosition <$> lookupDeclaration unDeclaration declScopeAddress g
  scope <- lookupScope declScopeAddress g
  pure $ insertScope declScopeAddress (scope { declarations = Seq.update position (declInfo { infoSpan = span }) (declarations scope) }) g

-- | Insert a new scope with the given address and edges into the scope graph.
newScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newScope address edges = insertScope address (Scope edges mempty mempty Standard)

-- | Insert a new scope with the given address and edges into the scope graph.
newPreludeScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newPreludeScope address edges = insertScope address (Scope edges mempty mempty Preluded)

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
  snd <$> Map.lookup ref pathsMap

-- Returns the scope the declaration was declared in.
scopeOfDeclaration :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
scopeOfDeclaration Declaration{..} g@(ScopeGraph graph) = go (Map.keys graph)
  where
    go = foldr (\ scope -> (scope <$ lookupDeclaration unDeclaration scope g <|>)) Nothing

-- | Returns the scope associated with a declaration (the child scope if any exists).
associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope Declaration{..} g@(ScopeGraph graph) = go (Map.keys graph)
  where
    go = foldr lookupAssociatedScope Nothing
    lookupAssociatedScope scope = ((lookupDeclaration unDeclaration scope g >>= infoAssociatedScope . fst) <|>)
