{-# LANGUAGE DeriveAnyClass, DerivingStrategies, GADTs #-}

module Data.Abstract.Environment
  ( Environment(..)
  , Bindings(..)
  , EvalContext(..)
  , EnvironmentError(..)
  , addresses
  , aliasBindings
  , allNames
  , delete
  , flatPairs
  , head
  , insert
  , insertEnv
  , intersect
  , lookup
  , lookupEnv'
  , names
  , newEnv
  , overwrite
  , pairs
  , pop
  , push
  , roots
  , unpairs
  ) where

import           Data.Abstract.Live
import           Data.Abstract.Name
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Prelude hiding (head, lookup)
import           Prologue

-- | A map of names to values. Represents a single scope level of an environment chain.
newtype Bindings address = Bindings { unBindings :: Map.Map Name address }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Semigroup (Bindings address) where
  (<>) (Bindings a) (Bindings b) = Bindings (a <> b)

instance Monoid (Bindings address) where
  mempty = Bindings mempty
  mappend = (<>)

instance Lower (Bindings address) where
  lowerBound = mempty

instance Show address => Show (Bindings address) where
  showsPrec d = showsUnaryWith showsPrec "Bindings" d . pairs


-- | A LIFO stack of maps of names to addresses, representing a lexically-scoped evaluation environment.
--   All behaviors can be assumed to be frontmost-biased: looking up "a" will check the most specific
--   scope for "a", then the next, and so on.
newtype Environment address = Environment { unEnvironment :: NonEmpty (Bindings address) }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

data EvalContext address = EvalContext { ctxSelf :: Maybe address, ctxEnvironment :: Environment address }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Errors involving the environment.
data EnvironmentError address return where
  FreeVariable :: Name -> EnvironmentError address address

instance NFData1 (EnvironmentError address) where
  liftRnf _ (FreeVariable n) = rnf n

instance (NFData return) => NFData (EnvironmentError address return) where
  rnf = liftRnf rnf

deriving instance Eq (EnvironmentError address return)
deriving instance Show (EnvironmentError address return)
instance Show1 (EnvironmentError address) where liftShowsPrec _ _ = showsPrec
instance Eq1 (EnvironmentError address) where liftEq _ (FreeVariable n1) (FreeVariable n2) = n1 == n2

instance Lower (EvalContext address) where
  lowerBound = EvalContext Nothing lowerBound

-- | Make and enter a new empty scope in the given environment.
push :: Environment address -> Environment address
push (Environment (a :| as)) = Environment (mempty :| a : as)

-- | Remove the frontmost scope.
pop :: Environment address -> Environment address
pop (Environment (_ :| []))     = lowerBound
pop (Environment (_ :| a : as)) = Environment (a :| as)

-- | Return the frontmost (ie. most local) frame of bindings in the environment
head :: Environment address -> Bindings address
head (Environment (a :| _)) = a

-- | Extract an association list of bindings from a 'Bindings'.
--
-- >>> pairs (head shadowed)
-- [("foo",Precise 1)]
pairs :: Bindings address -> [(Name, address)]
pairs = Map.toList . unBindings

unpairs :: [(Name, address)] -> Bindings address
unpairs = Bindings . Map.fromList

flatPairs :: Environment address -> [(Name, address)]
flatPairs = (>>= pairs) . toList . unEnvironment

newEnv :: Bindings address -> Environment address
newEnv = Environment . pure

-- | Lookup a 'Name' in the bindings.
lookup :: Name -> Bindings address -> Maybe address
lookup name = Map.lookup name . unBindings

-- | Lookup a 'Name' in the environment.
lookupEnv' :: Name -> Environment address -> Maybe address
lookupEnv' name = foldMapA (lookup name) . unEnvironment

-- | Insert a 'Name' in the bindings
insert :: Name -> address -> Bindings address -> Bindings address
insert name addr = Bindings . Map.insert name addr . unBindings

-- | Insert a 'Name' in the environment
insertEnv :: Name -> address -> Environment address -> Environment address
insertEnv name addr (Environment (Bindings a :| as)) = Environment (Bindings (Map.insert name addr a) :| as)

-- | Remove a 'Name' from the environment.
delete :: Name -> Environment address -> Environment address
delete name = trim . Environment . fmap (Bindings . Map.delete name . unBindings) . unEnvironment

trim :: Environment address -> Environment address
trim (Environment (a :| as)) = Environment (a :| filtered)
  where filtered = filter (not . Map.null . unBindings) as

intersect :: Foldable t => t Name -> Environment address -> Environment address
intersect names env = newEnv (unpairs (mapMaybe lookupName (toList names)))
  where
    lookupName name = (,) name <$> lookupEnv' name env

-- | Get all bound 'Name's in a binding.
names :: Bindings address -> [Name]
names = fmap fst . pairs

-- | Order preserving deduplication in O(n log n) time
dedup :: Ord a => [a] -> [a]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

-- | Get all bound 'Name's in an environment.
allNames :: Environment address -> [Name]
allNames = dedup . fmap fst . flatPairs

aliasBindings :: [(Name, Name)] -> Bindings address -> Bindings address
aliasBindings pairs binds = unpairs $ mapMaybe lookupAndAlias pairs
  where
    lookupAndAlias (oldName, newName) = (,) newName <$> Map.lookup oldName (unBindings binds)

-- | Lookup and alias name-value bindings from an environment.
overwrite :: [(Name, Name)] -> Environment address -> Environment address
overwrite pairs env = newEnv . unpairs $ mapMaybe lookupAndAlias pairs
  where
    lookupAndAlias (oldName, newName) = (,) newName <$> lookupEnv' oldName env

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord address, Foldable t) => Environment address -> t Name -> Live address
roots env names = addresses (names `intersect` env)

addresses :: Ord address => Environment address -> Live address
addresses = fromAddresses . map snd . flatPairs


instance Lower (Environment address) where lowerBound = Environment (lowerBound :| [])

-- N.B. this show instance drops some information to avoid generating
-- an infinite string in certain cases. As such, two unequal
-- environments may produce equal outputs over Show.
instance Show address => Show (Environment address) where
  showsPrec d = showsUnaryWith showsPrec "Environment" d . flatPairs
