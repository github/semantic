{-# LANGUAGE GADTs, DuplicateRecordFields #-}
module Data.Abstract.ScopeGraph
  ( ScopeGraph(..)
  , Path(..)
  , pathDeclaration
  , Reference(..) -- TODO don't export these constructors
  , Declaration(..) -- TODO don't export these constructors
  , EdgeLabel(..)
  , scopeOfRef
  , pathOfRef
  , declare
  , reference
  , newScope
  , associatedScope
  , insertDeclarationScope
  ) where

import           Data.Abstract.Name
import qualified Data.Map.Strict as Map
import           Data.Span
import           Prelude hiding (lookup)
import           Prologue

data Scope scopeAddress = Scope {
    edges        :: Map EdgeLabel [scopeAddress] -- Maybe Map EdgeLabel [Path scope]?
  , references   :: Map Reference (Path scopeAddress)
  , declarations :: Map Declaration (Span, Maybe scopeAddress)
  } deriving (Eq, Show, Ord)


data ScopeGraph scope = ScopeGraph { graph :: Map scope (Scope scope), currentScope :: Maybe scope }

instance Ord scope => Lower (ScopeGraph scope) where
  lowerBound = ScopeGraph mempty Nothing

deriving instance Eq address => Eq (ScopeGraph address)
deriving instance Show address => Show (ScopeGraph address)
deriving instance Ord address => Ord (ScopeGraph address)

data Path scope where
  -- | Construct a direct path to a declaration.
  DPath :: Declaration -> Path scope
  -- | Construct an edge from a scope to another declaration path.
  EPath :: EdgeLabel -> scope -> Path scope -> Path scope

deriving instance Eq scope => Eq (Path scope)
deriving instance Show scope => Show (Path scope)
deriving instance Ord scope => Ord (Path scope)

-- Returns the declaration of a path.
pathDeclaration :: Path scope -> Declaration
pathDeclaration (DPath d)     = d
pathDeclaration (EPath _ _ p) = pathDeclaration p

-- Returns the reference paths of a scope in a scope graph.
pathsOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Reference (Path scope))
pathsOfScope scope = fmap references . Map.lookup scope . graph

-- Returns the declaration data of a scope in a scope graph.
ddataOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map Declaration (Span, Maybe scope))
ddataOfScope scope = fmap declarations . Map.lookup scope . graph

-- Returns the edges of a scope in a scope graph.
linksOfScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Map EdgeLabel [scope])
linksOfScope scope = fmap edges . Map.lookup scope . graph

-- Lookup a scope in the scope graph.
lookupScope :: Ord scope => scope -> ScopeGraph scope -> Maybe (Scope scope)
lookupScope scope = Map.lookup scope . graph

-- Declare a declaration with a span and an associated scope in the scope graph.
declare :: Ord scope => Declaration -> Span -> Maybe scope -> ScopeGraph scope -> ScopeGraph scope
declare declaration ddata assocScope g@ScopeGraph{..} = fromMaybe g $ do
  scopeKey <- currentScope
  scope <- lookupScope scopeKey g
  let newScope = scope { declarations = Map.insert declaration (ddata, assocScope) (declarations scope) }
  pure $ g { graph = Map.insert scopeKey newScope graph }

-- | Add a reference to a declaration in the scope graph.
-- Returns the original scope graph if the declaration could not be found.
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
          Just _ ->
            let newScope = currentScope { references = Map.insert ref (path (DPath declaration)) (references currentScope) }
            in Just (g { graph = Map.insert currentAddress newScope graph })
          Nothing -> let
            traverseEdges edge = do
              linkMap <- linksOfScope address g
              scopes <- Map.lookup edge linkMap
              -- Return the first path to the declaration through the scopes.
              getFirst (foldMap (First . ap (go currentAddress currentScope) ((path .) . EPath edge)) scopes)
            in traverseEdges Import <|> traverseEdges Lexical

-- | Insert associate the given address to a declaration in the scope graph.
insertDeclarationScope :: Ord address => Declaration -> address -> ScopeGraph address -> ScopeGraph address
insertDeclarationScope decl address g@ScopeGraph{..} = fromMaybe g $ do
  declScope <- scopeOfDeclaration decl g
  scope <- lookupScope declScope g
  (span, _) <- Map.lookup decl (declarations scope)
  pure $ g { graph = Map.insert declScope (scope { declarations = Map.insert decl (span, Just address) (declarations scope) }) graph }

-- | Insert a new scope with the given address and edges into the scope graph.
newScope :: Ord address => address -> Map EdgeLabel [address] -> ScopeGraph address -> ScopeGraph address
newScope address edges g@ScopeGraph{..} = g { graph = Map.insert address newScope graph }
  where
    newScope = Scope edges mempty mempty

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
      _ <- Map.lookup declaration ddataMap
      pure (Just s)
    go [] = Nothing

-- | Returns the scope associated with a declaration (the child scope if any exists).
associatedScope :: Ord scope => Declaration -> ScopeGraph scope -> Maybe scope
associatedScope declaration g@ScopeGraph{..} = go (Map.keys graph)
  where
    go (s : scopes') = fromMaybe (go scopes') $ do
      ddataMap <- ddataOfScope s g
      (_, assocScope) <- Map.lookup declaration ddataMap
      pure assocScope
    go [] = Nothing

newtype Reference = Reference { name :: Name }
  deriving (Eq, Ord, Show)

newtype Declaration = Declaration { name :: Name }
  deriving (Eq, Ord, Show)

-- | The type of edge from a scope to its parent scopes.
-- Either a lexical edge or an import edge in the case of non-lexical edges.
data EdgeLabel = Lexical | Import
  deriving (Eq, Ord, Show)
