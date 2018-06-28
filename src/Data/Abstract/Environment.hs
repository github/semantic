module Data.Abstract.Environment
  ( Environment(..)
  , Bindings(..)
  , addresses
  , delete
  , flatPairs
  , head
  , insert
  , intersect
  , lookup
  , mergeEnvs
  , mergeNewer
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
import           Data.Align
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Prelude hiding (head, lookup)
import           Prologue

-- $setup
-- >>> import Data.Abstract.Address
-- >>> let bright = push (insert (name "foo") (Precise 0) lowerBound)
-- >>> let shadowed = insert (name "foo") (Precise 1) bright

-- | A map of names to values. Represents a single scope level of an environment chain.
newtype Bindings address = Bindings { unBindings :: Map.Map Name address }
  deriving (Eq, Ord)

instance Semigroup (Bindings address) where
  (<>) (Bindings a) (Bindings b) = Bindings (a <> b)

instance Monoid (Bindings address) where
  mempty = Bindings mempty
  mappend = (<>)

instance Lower (Bindings address) where
  lowerBound = mempty

-- | A LIFO stack of maps of names to addresses, representing a lexically-scoped evaluation environment.
--   All behaviors can be assumed to be frontmost-biased: looking up "a" will check the most specific
--   scope for "a", then the next, and so on.
newtype Environment address = Environment { unEnvironment :: NonEmpty (Bindings address) }
  deriving (Eq, Ord)

mergeEnvs :: Environment address -> Environment address -> Environment address
mergeEnvs (Environment (a :| as)) (Environment (b :| bs)) =
  Environment ((<>) a b :| alignWith (mergeThese (<>)) as bs)

-- | Make and enter a new empty scope in the given environment.
push :: Environment address -> Environment address
push (Environment (a :| as)) = Environment (mempty :| a : as)

-- | Remove the frontmost scope.
pop :: Environment address -> Environment address
pop (Environment (_ :| []))     = lowerBound
pop (Environment (_ :| a : as)) = Environment (a :| as)

-- | Drop all scopes save for the frontmost one.
head :: Environment address -> Environment address
head (Environment (a :| _)) = Environment (a :| [])

-- | Take the union of two environments. When duplicate keys are found in the
--   name to address map, the second definition wins.
mergeNewer :: Environment address -> Environment address -> Environment address
mergeNewer (Environment a) (Environment b) =
    Environment (NonEmpty.fromList . reverse . fmap Bindings $ alignWith (mergeThese combine) (reverse as) (reverse bs))
    where
      combine = Map.unionWith (flip const)
      as = unBindings <$> toList a
      bs = unBindings <$> toList b

-- | Extract an association list of bindings from an 'Environment'.
--
-- >>> pairs shadowed
-- [("foo",Precise 1)]
pairs :: Bindings address -> [(Name, address)]
pairs = Map.toList . unBindings

unpairs :: [(Name, address)] -> Bindings address
unpairs = Bindings . Map.fromList

flatPairs :: Environment address -> [(Name, address)]
flatPairs = (>>= pairs) . toList . unEnvironment

newEnv :: Bindings address -> Environment address
newEnv = Environment . pure

-- | Lookup a 'Name' in the environment.
--
-- >>> lookup (name "foo") shadowed
-- Just (Precise 1)
lookup :: Name -> Environment address -> Maybe address
lookup name = foldMapA (Map.lookup name) . fmap unBindings . unEnvironment

-- | Insert a 'Name' in the environment.
insert :: Name -> address -> Environment address -> Environment address
insert name addr (Environment ((Bindings a) :| as)) = Environment (Bindings (Map.insert name addr a) :| as)

-- | Remove a 'Name' from the environment.
--
-- >>> delete (name "foo") shadowed
-- Environment []
delete :: Name -> Environment address -> Environment address
delete name = trim . Environment . fmap (Bindings . Map.delete name) . fmap unBindings . unEnvironment

trim :: Environment address -> Environment address
trim (Environment (a :| as)) = Environment (a :| filtered)
  where filtered = filter (not . Map.null . unBindings) as

intersect :: Foldable t => t Name -> Environment address -> Environment address
intersect names env = newEnv (unpairs (mapMaybe lookupName (toList names)))
  where
    lookupName name = (,) name <$> lookup name env

-- | Get all bound 'Name's in an environment.
names :: Environment address -> [Name]
names = fmap fst . flatPairs

-- | Lookup and alias name-value bindings from an environment.
overwrite :: [(Name, Name)] -> Environment address -> Environment address
overwrite pairs env = newEnv . unpairs $ mapMaybe lookupAndAlias pairs
  where
    lookupAndAlias (oldName, newName) = (,) newName <$> lookup oldName env

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord address, Foldable t) => Environment address -> t Name -> Live address
roots env names = addresses (names `intersect` env)

addresses :: Ord address => Environment address -> Live address
addresses = fromAddresses . map snd . flatPairs


instance Lower (Environment address) where lowerBound = Environment (lowerBound :| [])

instance Show address => Show (Environment address) where
  showsPrec d = showsUnaryWith showsPrec "Environment" d . flatPairs
