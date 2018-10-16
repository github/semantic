{-# LANGUAGE GADTs, DuplicateRecordFields, TupleSections #-}
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
  , Path(..)
  , pathDeclaration
  , pathOfRef
  , pathPosition
  , Position(..)
  , reference
  , Reference(..) -- TODO don't export these constructors
  , ScopeGraph(..)
  , lookupScope
  , Scope(..)
  , scopeOfRef
  ) where

import           Data.Abstract.Name
import qualified Data.Map.Strict as Map
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue
import qualified Data.Sequence as Seq

data Address address = Address { address :: address, position :: Position }

-- Offsets and frame addresses in the heap should be addresses?
data Scope address = Scope {
    edges        :: Map EdgeLabel [address] -- Maybe Map EdgeLabel [Path scope]?
  , references   :: Map Reference (Path address)
  , declarations :: Seq (Declaration, (Span, Maybe address))
  } deriving (Eq, Show, Ord)

newtype Position = Position { unPosition :: Int }
  deriving (Eq, Show, Ord)

data ScopeGraph scope = ScopeGraph { graph :: Map scope (Scope scope), currentScope :: Maybe scope }

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty Nothing

deriving instance Eq address => Eq (ScopeGraph address)
deriving instance Show address => Show (ScopeGraph address)
deriving instance Ord address => Ord (ScopeGraph address)

data Path scope where
  -- | Construct a direct path to a declaration.
  DPath :: Declaration -> Position -> Path scope
  -- | Construct an edge from a scope to another declaration path.
  EPath :: EdgeLabel -> scope -> Path scope -> Path scope

deriving instance Eq scope => Eq (Path scope)
deriving instance Show scope => Show (Path scope)
deriving instance Ord scope => Ord (Path scope)

-- Returns the declaration of a path.
pathDeclaration :: Path scope -> Declaration
pathDeclaration (DPath d _)     = d
pathDeclaration (EPath _ _ p) = pathDeclaration p

pathPosition :: Path scope -> Position
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
declare :: Ord scope => Declaration -> Span -> Maybe scope -> ScopeGraph scope -> (ScopeGraph scope, Maybe Position)
declare declaration ddata assocScope g@ScopeGraph{..} = fromMaybe (g, Nothing) $ do
  scopeKey <- currentScope
  scope <- lookupScope scopeKey g
  let newScope = scope { declarations = (declarations scope) Seq.|> (declaration, (ddata, assocScope)) }
  pure $ (g { graph = Map.insert scopeKey newScope graph }, Just . Position $ length (declarations newScope))

-- | Add a reference to a declaration in the scope graph.
-- Returns the original scope graph if the declaration could not be found.
reference :: Ord scope => Reference -> Declaration -> ScopeGraph scope -> ScopeGraph scope
reference ref declaration g@ScopeGraph{..} = fromMaybe g $ do
  -- Start from the current address
  currentAddress <- currentScope
  currentScope' <- lookupScope currentAddress g
  -- Build a path up to the declaration
  go currentAddress currentScope' currentAddress id
  where
    declDataOfScope address = do
      dataMap <- ddataOfScope address g
      lookupDeclaration declaration dataMap
    go currentAddress currentScope address path =
      case declDataOfScope address of
          Just (_, index) ->
            let newScope = currentScope { references = Map.insert ref (path (DPath declaration index)) (references currentScope) }
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
insertImportReference ref decl g@ScopeGraph{..} scopeAddress scope = do
  currentAddress <- currentScope
  go currentAddress (EPath Import scopeAddress)
  where
    go address path =
      case declDataOfScope address of
        Just (_, index) ->
          Just $ scope { references = Map.insert ref (path (DPath decl index)) (references scope) }
        Nothing -> traverseEdges Import <|> traverseEdges Lexical
          where
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              -- Return the first path to the declaration through the scopes.
              getFirst (foldMap (First . ap go ((path .) . EPath edge)) scopes)
    declDataOfScope address = do
      dataMap <- ddataOfScope address g
      lookupDeclaration decl dataMap

lookupDeclaration :: Declaration -> Seq (Declaration, (Span, Maybe address)) -> Maybe ((Declaration, (Span, Maybe address)), Position)
lookupDeclaration declaration seq = do
      index <- Seq.findIndexR ((declaration ==) . fst) seq
      (, Position index) <$> Seq.lookup index seq

-- | Insert associate the given address to a declaration in the scope graph.
insertDeclarationScope :: Ord address => Declaration -> address -> ScopeGraph address -> ScopeGraph address
insertDeclarationScope decl address g@ScopeGraph{..} = fromMaybe g $ do
  declScope <- scopeOfDeclaration decl g
  scope <- lookupScope declScope g
  (span, position) <- (fst . snd . fst &&& unPosition . snd) <$> lookupDeclaration decl (declarations scope)
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
scopeOfDeclaration declaration g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      ddataMap <- ddataOfScope s g
      _ <- lookupDeclaration declaration ddataMap
      pure (Just s)
    go [] = Nothing

-- | Returns the scope associated with a declaration (the child scope if any exists).
associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope declaration g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      ddataMap <- ddataOfScope s g
      snd . snd . fst <$> lookupDeclaration declaration ddataMap
    go [] = Nothing

newtype Reference = Reference { name :: Name }
  deriving (Eq, Ord, Show)

newtype Declaration = Declaration { name :: Name }
  deriving (Eq, Ord, Show)

-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import
  deriving (Eq, Ord, Show)
