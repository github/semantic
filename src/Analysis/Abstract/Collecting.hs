{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Abstract.Collecting where

import Control.Monad.Effect.GC
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Semigroup

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
  roots <- askRoots :: m (Live (LocationFor v) v)
  v <- ev0 ev' yield e
  modifyStore (gc (roots <> valueRoots v))
  return v

gc :: (Ord (LocationFor a), Foldable (Cell (LocationFor a)), ValueRoots (LocationFor a) a) => Live (LocationFor a) a -> Store (LocationFor a) a -> Store (LocationFor a) a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord (LocationFor a), Foldable (Cell (LocationFor a)), ValueRoots (LocationFor a) a) => Live (LocationFor a) a -> Store (LocationFor a) a -> Live (LocationFor a) a
reachable roots store = go roots mempty
  where go set seen = case liveSplit set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen) (liveInsert a seen)
            | otherwise -> go seen (liveInsert a seen)
