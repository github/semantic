{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Environment
  ( Environment(..)
  , addresses
  , bind
  , delete
  , head
  , emptyEnv
  , mergeEnvs
  , mergeNewer
  , insert
  , lookup
  , names
  , overwrite
  , pairs
  , unpairs
  , pop
  , push
  , roots
  ) where

import           Prelude hiding (head, lookup)
import           Data.Abstract.Address
import           Data.Abstract.FreeVariables
import           Data.Abstract.Live
import           Data.Align
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           GHC.Exts (IsList (..))
import           Prologue
import qualified Data.List.NonEmpty as NonEmpty

-- $setup
-- >>> let bright = push (insert (name "foo") (Address (Precise 0)) emptyEnv)
-- >>> let shadowed = insert (name "foo") (Address (Precise 1)) bright

-- | A LIFO stack of maps of names to addresses, representing a lexically-scoped evaluation environment.
--   All behaviors can be assumed to be frontmost-biased: looking up "a" will check the most specific
--   scope for "a", then the next, and so on.
newtype Environment location value = Environment { unEnvironment :: NonEmpty (Map.Map Name location) }
  deriving (Eq, Ord)

instance Eq   location => Eq1   (Environment location) where liftEq      _ (Environment a) (Environment b) = a == b
instance Ord  location => Ord1  (Environment location) where liftCompare _ (Environment a) (Environment b) = a `compare` b
instance Show location => Show1 (Environment location) where liftShowsPrec _ _ = showsPrec

-- | The provided list will be put into an Environment with one member, so fromList is total
--   (despite NonEmpty's instance being partial). Don't pass in multiple Addresses for the
--   same Name or you violate the axiom that toList . fromList == id.
instance IsList (Environment location value) where
  type Item (Environment location value) = (Name, Address location value)
  fromList xs                   = Environment (Map.fromList (second unAddress <$> xs) :| [])
  toList (Environment (x :| _)) = second Address <$> Map.toList x

mergeEnvs :: Environment location value -> Environment location value -> Environment location value
mergeEnvs (Environment (a :| as)) (Environment (b :| bs)) =
  Environment ((<>) a b :| alignWith (mergeThese (<>)) as bs)

emptyEnv :: Environment location value
emptyEnv = Environment (lowerBound :| [])

-- | Make and enter a new empty scope in the given environment.
push :: Environment location value -> Environment location value
push (Environment (a :| as)) = Environment (mempty :| a : as)

-- | Remove the frontmost scope.
pop :: Environment location value -> Environment location value
pop (Environment (_ :| []))     = emptyEnv
pop (Environment (_ :| a : as)) = Environment (a :| as)

-- | Drop all scopes save for the frontmost one.
head :: Environment location value -> Environment location value
head (Environment (a :| _)) = Environment (a :| [])

-- | Take the union of two environments. When duplicate keys are found in the
--   name to address map, the second definition wins.
mergeNewer :: Environment location value -> Environment location value -> Environment location value
mergeNewer (Environment a) (Environment b) =
    Environment (NonEmpty.fromList . reverse $ alignWith (mergeThese combine) (reverse as) (reverse bs))
    where
      combine = Map.unionWith (flip const)
      as = NonEmpty.toList a
      bs = NonEmpty.toList b

-- | Extract an association list of bindings from an 'Environment'.
--
-- >>> pairs shadowed
-- [(Name {unName = "foo"},Address (Precise 1))]
pairs :: Environment location value -> [(Name, Address location value)]
pairs = map (second Address) . Map.toList . fold . unEnvironment

unpairs :: [(Name, Address location value)] -> Environment location value
unpairs = fromList

-- | Lookup a 'Name' in the environment.
--
-- >>> lookup (name "foo") shadowed
-- Just (Address (Precise 1))
lookup :: Name -> Environment location value -> Maybe (Address location value)
lookup k = fmap Address . foldMapA (Map.lookup k) . unEnvironment

-- | Insert a 'Name' in the environment.
insert :: Name -> Address location value -> Environment location value -> Environment location value
insert name (Address value) (Environment (a :| as)) = Environment (Map.insert name value a :| as)

-- | Remove a 'Name' from the environment.
--
-- >>> delete (name "foo") shadowed
-- Environment (fromList [] :| [])
delete :: Name -> Environment location value -> Environment location value
delete name = trim . Environment . fmap (Map.delete name) . unEnvironment

trim :: Environment location value -> Environment location value
trim (Environment (a :| as)) = Environment (a :| filtered)
  where filtered = filter (not . Map.null) as

bind :: Foldable t => t Name -> Environment location value -> Environment location value
bind names env = fromList (mapMaybe lookupName (Prologue.toList names))
  where
    lookupName name = (,) name <$> lookup name env

-- | Get all bound 'Name's in an environment.
names :: Environment location value -> [Name]
names = fmap fst . pairs

-- | Lookup and alias name-value bindings from an environment.
overwrite :: [(Name, Name)] -> Environment location value -> Environment location value
overwrite pairs env = fromList $ mapMaybe lookupAndAlias pairs
  where
    lookupAndAlias (oldName, newName) = (,) newName <$> lookup oldName env

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord location, Foldable t) => Environment location value -> t Name -> Live location value
roots env = foldMap (maybe mempty liveSingleton . flip lookup env)

addresses :: Ord location => Environment location value -> Live location value
addresses = fromAddresses . map snd . pairs


instance Lower (Environment location value) where lowerBound = emptyEnv

instance Show location => Show (Environment location value) where
  showsPrec d = showsUnaryWith showsPrec "Environment" d . map (first unName) . pairs
