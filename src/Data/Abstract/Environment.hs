{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies #-}
module Data.Abstract.Environment
  ( Environment
  , bindEnv
  , delete
  , envAll
  , head
  , insert
  , lookup
  , names
  , pairs
  , pop
  , push
  , rename
  , roots
  ) where

import           Prelude hiding (head, lookup)
import           Data.Abstract.Address
import           Data.Abstract.FreeVariables
import           Data.Abstract.Live
import           Data.Align
import qualified Data.Map as Map
import           Data.Semigroup.Reducer
import qualified Data.Set as Set
import           GHC.Exts (IsList (..))
import           Prologue

-- | A map of names to addresses that represents the evaluation environment.
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

instance Semigroup (Environment l a) where
  Environment (a :| as) <> Environment (b :| bs) =
    Environment ((a <> b) :| alignWith (mergeThese (<>)) as bs)

instance Reducer (Name, Address l a) (Environment l a) where
  unit a = Environment (unit a :| [])

-- Possibly unlawful. If this breaks, you get to keep both pieces.
instance Monoid (Environment l a) where
  mappend = (<>)
  mempty  = Environment (mempty :| [])

push :: Environment l a -> Environment l a
push (Environment (a :| as)) = Environment (mempty :| a : as)

pop :: Environment l a -> Environment l a
pop (Environment (_ :| []))     = mempty
pop (Environment (_ :| a : as)) = Environment (a :| as)

head :: Environment l a -> Environment l a
head (Environment (a :| _)) = Environment (a :| [])

-- TODO: Test the flattening behavior
pairs :: Environment l a -> [(Name, Address l a)]
pairs = Map.toList . fold . unEnvironment

-- | Lookup a 'Name' in the environment.
lookup :: Name -> Environment l a -> Maybe (Address l a)
lookup k = foldMapA (Map.lookup k) . unEnvironment

-- | Insert a 'Name' in the environment.
insert :: Name -> Address l a -> Environment l a -> Environment l a
insert name value (Environment (a :| as)) = Environment (Map.insert name value a :| as)

delete :: Name -> Environment l a -> Environment l a
delete name = Environment . fmap (Map.delete name) . unEnvironment

bindEnv :: (Ord l, Foldable t) => t Name -> Environment l a -> Environment l a
bindEnv names env = foldMap envForName names
  where envForName name = maybe mempty (curry unit name) (lookup name env)

names :: Environment l a -> [Name]
names = fmap fst . pairs

-- TODO: rename this, because it both filters and renames
rename :: [(Name, Name)] -> Environment l a -> Environment l a
rename pairs env = foldMap rename pairs where
  rename (k, v) = case lookup k env of
    Nothing   -> mempty
    Just addr -> unit (v, addr)

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
roots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
roots env = foldMap (maybe mempty liveSingleton . flip lookup env)

-- TODO, VERY BROKEN, DON'T COMMIT THIS: needs to prefer inwardly-bound names to handle scoping
envAll :: (Ord l) => Environment l a -> Live l a
envAll (Environment env) = Live $ Set.fromList (foldMap Map.elems env)
