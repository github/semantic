{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Abstract.Collecting where

import Control.Monad.Effect.Env
import Control.Monad.Effect.GC
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Eval
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Semigroup
import Data.Set hiding (foldr)

evCollect :: forall t v m
          .  ( Ord (LocationFor v)
             , Foldable (Cell (LocationFor v))
             , MonadStore v m
             , MonadGC v m
             , ValueRoots (LocationFor v) v
             )
          => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
          -> ((v -> m v) -> t -> m v)
          -> (v -> m v) -> t -> m v
evCollect ev0 ev' yield e = do
  roots <- askRoots :: m (Set (Address (LocationFor v) v))
  v <- ev0 ev' yield e
  modifyStore (gc (roots <> valueRoots v))
  return v

evRoots :: forall v m term
        .  ( Ord (LocationFor v)
           , MonadEnv v m
           , MonadGC v m
           , ValueRoots (LocationFor v) v
           , Recursive term
           , Functor (Base term)
           , Eval term v m (Base term)
           )
        => ((v -> m v) -> term -> m v)
        -> (v -> m v) -> term -> m v
evRoots ev' yield = eval ev' yield . project

gc :: (Ord (LocationFor a), Foldable (Cell (LocationFor a)), ValueRoots (LocationFor a) a) => Set (Address (LocationFor a) a) -> Store (LocationFor a) a -> Store (LocationFor a) a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord (LocationFor a), Foldable (Cell (LocationFor a)), ValueRoots (LocationFor a) a) => Set (Address (LocationFor a) a) -> Store (LocationFor a) a -> Set (Address (LocationFor a) a)
reachable roots store = go roots mempty
  where go set seen = case minView set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (difference (foldr ((<>) . valueRoots) mempty values <> as) seen) (insert a seen)
            | otherwise -> go seen (insert a seen)
