{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies #-}
module Data.Abstract.Environment
  ( Environment
  , addresses
  , bind
  , delete
  , head
  , insert
  , lookup
  , names
  , overwrite
  , pairs
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
import           Data.Semigroup.Reducer
import           GHC.Exts (IsList (..))
import           Prologue

-- $setup
-- >>> let bright = push (insert (name "foo") (Address (Precise 0)) mempty)
-- >>> let shadowed = insert (name "foo") (Address (Precise 1)) bright

-- | A LIFO stack of maps of names to addresses, representing a lexically-scoped evaluation environment.
--   All behaviors can be assumed to be frontmost-biased: looking up "a" will check the most specific
--   scope for "a", then the next, and so on.
newtype Environment l a = Environment { unEnvironment :: NonEmpty (Map.Map Name (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec

-- | The provided list will be put into an Environment with one member, so fromList is total
--   (despite NonEmpty's instance being partial). Don't pass in multiple Addresses for the
--   same Name or you violate the axiom that toList . fromList == id.
instance IsList (Environment l a) where
  type Item (Environment l a) = (Name, Address l a)
  fromList xs                   = Environment (Map.fromList xs :| [])
  toList (Environment (x :| _)) = Map.toList x

-- TODO: property-check me
instance Semigroup (Environment l a) where
  Environment (a :| as) <> Environment (b :| bs) =
    Environment ((a <> b) :| alignWith (mergeThese (<>)) as bs)

instance Reducer (Name, Address l a) (Environment l a) where
  unit a = Environment (unit a :| [])

-- | This instance is possibly unlawful. If this breaks, you get to keep both pieces.
instance Monoid (Environment l a) where
  mappend = (<>)
  mempty  = Environment (mempty :| [])

-- | Make and enter a new empty scope in the given environment.
push :: Environment l a -> Environment l a
push (Environment (a :| as)) = Environment (mempty :| a : as)

-- | Remove the frontmost scope.
pop :: Environment l a -> Environment l a
pop (Environment (_ :| []))     = mempty
pop (Environment (_ :| a : as)) = Environment (a :| as)

-- | Drop all scopes save for the frontmost one.
head :: Environment l a -> Environment l a
head (Environment (a :| _)) = Environment (a :| [])

-- | Extract an association list of bindings from an 'Environment'.
--
-- >>> pairs shadowed
-- [("foo" :| [],Address {unAddress = Precise {unPrecise = 1}})]
pairs :: Environment l a -> [(Name, Address l a)]
pairs = Map.toList . fold . unEnvironment

-- | Lookup a 'Name' in the environment.
--
-- >>> lookup (name "foo") shadowed
-- Just (Address {unAddress = Precise {unPrecise = 1}})
lookup :: Name -> Environment l a -> Maybe (Address l a)
lookup k = foldMapA (Map.lookup k) . unEnvironment

-- | Insert a 'Name' in the environment.
insert :: Name -> Address l a -> Environment l a -> Environment l a
insert name value (Environment (a :| as)) = Environment (Map.insert name value a :| as)

-- | Remove a 'Name' from the environment.
--
-- >>> delete (name "foo") shadowed
-- Environment {unEnvironment = fromList [] :| []}
delete :: Name -> Environment l a -> Environment l a
delete name = trim . Environment . fmap (Map.delete name) . unEnvironment

trim :: Environment l a -> Environment l a
trim (Environment (a :| as)) = Environment (a :| filtered)
  where filtered = filter (not . Map.null) as

bind :: Foldable t => t Name -> Environment l a -> Environment l a
bind names env = foldMap envForName names
  where envForName name = maybe mempty (curry unit name) (lookup name env)

-- | Get all bound 'Name's in an environment.
names :: Environment l a -> [Name]
names = fmap fst . pairs

-- | Overwrite a set of key-value bindings in the provided environment.
overwrite :: [(Name, Name)] -> Environment l a -> Environment l a
overwrite pairs env = foldMap go pairs where
  go (k, v) = case lookup k env of
    Nothing   -> mempty
    Just addr -> unit (v, addr)

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
roots env = foldMap (maybe mempty liveSingleton . flip lookup env)

addresses :: Ord l => Environment l a -> Live l a
addresses = Live . fromList . fmap snd . pairs
