module Data.Abstract.Environment
  ( Environment(..)
  , addresses
  , delete
  , head
  , emptyEnv
  , mergeEnvs
  , mergeNewer
  , insert
  , lookup
  , names
  , intersect
  , overwrite
  , pairs
  , unpairs
  , pop
  , push
  , roots
  ) where

import           Data.Abstract.Live
import           Data.Abstract.Name
import           Data.Align
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Prelude hiding (head, lookup)
import           Prologue

-- $setup
-- >>> import Data.Abstract.Address
-- >>> let bright = push (insert (name "foo") (Precise 0) emptyEnv)
-- >>> let shadowed = insert (name "foo") (Precise 1) bright

-- | A LIFO stack of maps of names to addresses, representing a lexically-scoped evaluation environment.
--   All behaviors can be assumed to be frontmost-biased: looking up "a" will check the most specific
--   scope for "a", then the next, and so on.
newtype Environment location = Environment { unEnvironment :: NonEmpty (Map.Map Name location) }
  deriving (Eq, Ord)

mergeEnvs :: Environment location -> Environment location -> Environment location
mergeEnvs (Environment (a :| as)) (Environment (b :| bs)) =
  Environment ((<>) a b :| alignWith (mergeThese (<>)) as bs)

emptyEnv :: Environment location
emptyEnv = Environment (lowerBound :| [])

-- | Make and enter a new empty scope in the given environment.
push :: Environment location -> Environment location
push (Environment (a :| as)) = Environment (mempty :| a : as)

-- | Remove the frontmost scope.
pop :: Environment location -> Environment location
pop (Environment (_ :| []))     = emptyEnv
pop (Environment (_ :| a : as)) = Environment (a :| as)

-- | Drop all scopes save for the frontmost one.
head :: Environment location -> Environment location
head (Environment (a :| _)) = Environment (a :| [])

-- | Take the union of two environments. When duplicate keys are found in the
--   name to address map, the second definition wins.
mergeNewer :: Environment location -> Environment location -> Environment location
mergeNewer (Environment a) (Environment b) =
    Environment (NonEmpty.fromList . reverse $ alignWith (mergeThese combine) (reverse as) (reverse bs))
    where
      combine = Map.unionWith (flip const)
      as = toList a
      bs = toList b

-- | Extract an association list of bindings from an 'Environment'.
--
-- >>> pairs shadowed
-- [("foo",Precise 1)]
pairs :: Environment location -> [(Name, location)]
pairs = Map.toList . fold . unEnvironment

unpairs :: [(Name, location)] -> Environment location
unpairs = Environment . pure . Map.fromList

-- | Lookup a 'Name' in the environment.
--
-- >>> lookup (name "foo") shadowed
-- Just (Precise 1)
lookup :: Name -> Environment location -> Maybe location
lookup name = foldMapA (Map.lookup name) . unEnvironment

-- | Insert a 'Name' in the environment.
insert :: Name -> location -> Environment location -> Environment location
insert name addr (Environment (a :| as)) = Environment (Map.insert name addr a :| as)

-- | Remove a 'Name' from the environment.
--
-- >>> delete (name "foo") shadowed
-- Environment []
delete :: Name -> Environment location -> Environment location
delete name = trim . Environment . fmap (Map.delete name) . unEnvironment

trim :: Environment location -> Environment location
trim (Environment (a :| as)) = Environment (a :| filtered)
  where filtered = filter (not . Map.null) as

intersect :: Foldable t => t Name -> Environment location -> Environment location
intersect names env = unpairs (mapMaybe lookupName (toList names))
  where
    lookupName name = (,) name <$> lookup name env

-- | Get all bound 'Name's in an environment.
names :: Environment location -> [Name]
names = fmap fst . pairs

-- | Lookup and alias name-value bindings from an environment.
overwrite :: [(Name, Name)] -> Environment location -> Environment location
overwrite pairs env = unpairs $ mapMaybe lookupAndAlias pairs
  where
    lookupAndAlias (oldName, newName) = (,) newName <$> lookup oldName env

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord location, Foldable t) => Environment location -> t Name -> Live location
roots env names = addresses (names `intersect` env)

addresses :: Ord location => Environment location -> Live location
addresses = fromAddresses . map snd . pairs


instance Lower (Environment location) where lowerBound = emptyEnv

instance Show location => Show (Environment location) where
  showsPrec d = showsUnaryWith showsPrec "Environment" d . pairs
