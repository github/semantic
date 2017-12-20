{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications #-}
module Analysis.Abstract.Caching where

import Analysis.Abstract.Collecting
import Control.Applicative
import Control.Effect
import Control.Monad.Effect.Address
import Control.Monad.Effect.Cache
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Maybe
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set

type CachingInterpreter t v
  = '[ Fresh
     , Reader (Live (LocationFor v) v)
     , Reader (Environment (LocationFor v) v)
     , Fail
     , NonDetEff
     , State (Store (LocationFor v) v)
     , Reader (Cache (LocationFor v) t v)
     , State (Cache (LocationFor v) t v)
     ]

type CachingResult t v = Final (CachingInterpreter t v) v

type MonadCachingInterpreter t v m
  = ( MonadEnv v m
    , MonadStore v m
    , MonadCacheIn t v m
    , MonadCacheOut t v m
    , MonadGC v m
    , Alternative m
    )


-- | Coinductively-cached evaluation.
evalCache :: forall v term
          . ( Ord v
            , Ord term
            , Ord (LocationFor v)
            , Ord (Cell (LocationFor v) v)
            , Foldable (Cell (LocationFor v))
            , Functor (Base term)
            , Recursive term
            , MonadAddress (LocationFor v) (Eff (CachingInterpreter term v))
            , Semigroup (Cell (LocationFor v) v)
            , ValueRoots (LocationFor v) v
            , Eval term v (Eff (CachingInterpreter term v)) (Base term)
            )
          => term
          -> CachingResult term v
evalCache e = run @(CachingInterpreter term v) (fixCache (fix (evCache (evCollect (\ recur yield -> eval recur yield . project)))) pure e)


evCache :: forall t v m
        . ( Ord (LocationFor v)
          , Ord t
          , Ord v
          , Ord (Cell (LocationFor v) v)
          , MonadCachingInterpreter t v m
          )
        => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
        -> ((v -> m v) -> t -> m v)
        -> (v -> m v) -> t -> m v
evCache ev0 ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e roots env store :: Configuration (LocationFor v) t v
  out <- getCache
  case cacheLookup c out of
    Just pairs -> asum . flip map (toList pairs) $ \ (value, store') -> do
      putStore store'
      pure value
    Nothing -> do
      in' <- askCache
      let pairs = fromMaybe mempty (cacheLookup c in')
      putCache (cacheSet c pairs out)
      v <- ev0 ev' yield e
      store' <- getStore
      modifyCache (cacheInsert c (v, store'))
      pure v

fixCache :: forall t v m
         . ( Ord (LocationFor v)
           , Ord t
           , Ord v
           , Ord (Cell (LocationFor v) v)
           , MonadCachingInterpreter t v m
           , MonadNonDet m
           , MonadFresh m
           )
         => ((v -> m v) -> t -> m v)
         -> (v -> m v) -> t -> m v
fixCache ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e roots env store :: Configuration (LocationFor v) t v
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache (LocationFor v) t v)
    putStore store
    reset 0
    _ <- localCache (const dollar) (collect point (ev' yield e) :: m (Set.Set v))
    getCache)
  asum . flip map (maybe [] toList (cacheLookup c pairs)) $ \ (value, store') -> do
    putStore store'
    pure value


mlfp :: (Eq a, Monad m) => a -> (a -> m a) -> m a
mlfp a f = loop a
  where loop x = do
          x' <- f x
          if x' == x then
            pure x
          else
            loop x'
