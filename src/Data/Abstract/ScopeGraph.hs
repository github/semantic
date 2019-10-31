{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DuplicateRecordFields, LambdaCase, OverloadedStrings, RecordWildCards, TupleSections #-}
module Data.Abstract.ScopeGraph
  ( Slot(..)
  , Info(..)
  , associatedScope
  , lookupDeclaration
  , declarationByName
  , declarationsByAccessControl
  , declarationsByRelation
  , Declaration(..) -- TODO don't export these constructors
  , declare
  , formatDeclaration
  , EdgeLabel(..)
  , insertDeclarationScope
  , insertDeclarationSpan
  , insertImportReference
  , newScope
  , newPreludeScope
  , insertScope
  , insertEdge
  , Path(..)
  , pathDeclaration
  , pathOfRef
  , pathPosition
  , Position(..)
  , reference
  , Reference(..) -- TODO don't export these constructors
  , ReferenceInfo(..)
  , Relation(..)
  , ScopeGraph(..)
  , Kind(..)
  , lookupScope
  , lookupScopePath
  , Scope(..)
  , scopeOfRef
  , pathDeclarationScope
  , putDeclarationScopeAtPosition
  , declarationNames
  , AccessControl(..)
  ) where

import Prelude hiding (lookup)
import Prologue

import           Control.Lens.Lens
import           Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Control.Abstract.Hole
import Data.Abstract.Module
import Data.JSON.Fields
import Data.Abstract.Name
import Source.Span

-- A slot is a location in the heap where a value is stored.
data Slot address = Slot { frameAddress :: address, position :: Position }
    deriving (Eq, Show, Ord)


data AccessControl = Public
                   | Protected
                   | Private
                   deriving (Bounded, Enum, Eq, Generic, Hashable, ToJSON, Show)

instance ToJSONFields AccessControl where
  toJSONFields accessControl = ["accessControl" .= accessControl]

-- | The Ord AccessControl instance represents an order specification of AccessControls.
-- AccessControls that are less than or equal to another AccessControl implies access.
-- It is helpful to consider `Public <= Private` as saying "Can a Public syntax term access a Private syntax term?"
-- In this way, Public AccessControl is the top of the order specification, and Private AccessControl is the bottom.
instance Ord AccessControl where
  -- | Private AccessControl represents the least overlap or accessibility with other AccessControls.
  -- When asking if the AccessControl "on the left" is less than the AccessControl "on the right", Private AccessControl on the left always implies access to the thing on the right.
  (<=) Private _           = True
  (<=) _       Private     = False

  -- | Protected AccessControl is in between Private and Public in the order specification.
  -- Protected AccessControl "on the left" has access to Protected or Public AccessControls "on the right".
  (<=) Protected Public    = True
  (<=) Protected Protected = True

  -- | Public AccessControl "on the left" has access only to Public AccessControl "on the right".
  (<=) Public Public       = True
  (<=) Public _            = False


data Relation = Default | Instance | Prelude | Gensym
  deriving (Bounded, Enum, Eq, Show, Ord)

instance Lower Relation where
  lowerBound = Default

data Info scopeAddress = Info
  { infoDeclaration     :: Declaration
  , infoModule          :: ModuleInfo
  , infoRelation        :: Relation
  , infoAccessControl   :: AccessControl
  , infoSpan            :: Span
  , infoKind            :: Kind
  , infoAssociatedScope :: Maybe scopeAddress
  } deriving (Eq, Show, Ord)

instance HasSpan (Info scopeAddress) where
  span_ = lens infoSpan (\i s -> i { infoSpan = s })
  {-# INLINE span_ #-}

instance Lower (Info scopeAddress) where
  lowerBound = Info lowerBound lowerBound lowerBound Public lowerBound lowerBound Nothing

data ReferenceInfo = ReferenceInfo
  { refSpan   :: Span
  , refKind   :: Kind
  , refModule :: ModuleInfo
  } deriving (Eq, Show, Ord)

instance HasSpan ReferenceInfo where
  span_ = lens refSpan (\r s -> r { refSpan = s })
  {-# INLINE span_ #-}

data Kind = AbstractClass
          | Assignment
          | Call
          | Class
          | DefaultExport
          | Function
          | Identifier
          | Let
          | MemberAccess
          | Method
          | Module
          | New
          | Parameter
          | PublicField
          | QualifiedAliasedImport
          | QualifiedExport
          | QualifiedImport
          | RequiredParameter
          | This
          | TypeAlias
          | TypeIdentifier
          | Unknown
          | UnqualifiedImport
          | VariableDeclaration
  deriving (Bounded, Enum, Eq, Show, Ord)

instance Lower Kind where
  lowerBound = Unknown

-- Offsets and frame addresses in the heap should be addresses?
data Scope address =
    Scope {
      edges        :: Map EdgeLabel [address]
    , references   :: Map Reference ([ReferenceInfo], Path address)
    , declarations :: Seq (Info address)
    }
  | PreludeScope {
      edges        :: Map EdgeLabel [address]
    , references   :: Map Reference ([ReferenceInfo], Path address)
    , declarations :: Seq (Info address)
    }
  deriving (Eq, Show, Ord)

instance Lower (Scope scopeAddress) where
  lowerBound = Scope mempty mempty mempty

instance AbstractHole (Scope scopeAddress) where
  hole = lowerBound

instance AbstractHole address => AbstractHole (Slot address) where
  hole = Slot hole (Position 0)

instance AbstractHole (Info address) where
  hole = lowerBound

newtype Position = Position { unPosition :: Int }
  deriving (Eq, Show, Ord)

newtype ScopeGraph scope = ScopeGraph { unScopeGraph :: Map scope (Scope scope) }
  deriving (Eq, Ord, Show)

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty

data Path scope
  = Hole
  -- | Construct a direct path to a declaration.
  | DPath Declaration Position
  -- | Construct an edge from a scope to another declaration path.
  | EPath EdgeLabel scope (Path scope)
  deriving (Eq, Functor, Ord, Show)

instance AbstractHole (Path scope) where
  hole = Hole

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

lookupReference :: Ord scopeAddress => Name -> scopeAddress -> ScopeGraph scopeAddress -> Maybe (Path scopeAddress)
lookupReference  name scope g = fmap snd . Map.lookup (Reference name) =<< pathsOfScope scope g

insertEdge :: Ord scopeAddress => EdgeLabel -> scopeAddress -> scopeAddress -> ScopeGraph scopeAddress -> ScopeGraph scopeAddress
insertEdge label target currentAddress g@(ScopeGraph graph) = fromMaybe g $ do
  currentScope' <- lookupScope currentAddress g
  scopes <- maybeM (Just mempty) (Map.lookup label (edges currentScope'))
  let newScope = currentScope' { edges = Map.insert label (target : scopes) (edges currentScope') }
  pure (ScopeGraph (Map.insert currentAddress newScope graph))


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
newScope address edges = insertScope address (Scope edges mempty mempty)

-- | Insert a new scope with the given address and edges into the scope graph.
newPreludeScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newPreludeScope address edges = insertScope address (PreludeScope edges mempty mempty)

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

newtype Reference = Reference { unReference :: Name }
  deriving (Eq, Ord, Show)

instance Lower Reference where
  lowerBound = Reference $ name ""

newtype Declaration = Declaration { unDeclaration :: Name }
  deriving (Eq, Ord, Show)

instance Lower Declaration where
  lowerBound = Declaration $ name ""

formatDeclaration :: Declaration -> Text
formatDeclaration = formatName . unDeclaration

-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import | Export | Superclass
  deriving (Bounded, Enum, Eq, Ord, Show)
