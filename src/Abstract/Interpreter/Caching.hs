{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Interpreter
import Control.Applicative
import Control.Effect
import Control.Monad.Effect.Address
import Control.Monad.Effect.Cache
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Store
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.FreeVariables
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import Data.Maybe
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term

type CachingInterpreter t v = '[Fresh, Reader (Set.Set (Address (LocationFor v) v)), Reader (Environment (LocationFor v) v), Fail, NonDetEff, State (Store (LocationFor v) v), Reader (Cache (LocationFor v) t v), State (Cache (LocationFor v) t v)]

type CachingResult t v = Final (CachingInterpreter t v) v

type MonadCachingInterpreter t v m = (MonadEnv v m, MonadStore v m, MonadCacheIn t v m, MonadCacheOut t v m, MonadGC v m, Alternative m)


class (Alternative m, Monad m) => MonadNonDet m where
  collect :: Monoid b => (a -> b) -> m a -> m b

instance (NonDetEff :< fs) => MonadNonDet (Eff fs) where
  collect f = interpose (pure . f) (\ m k -> case m of
    MZero -> pure mempty
    MPlus -> mappend <$> k True <*> k False)


-- Coinductively-cached evaluation
--
-- Examples:
--    evalCache @Type <term>
--    evalCache @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise) <term>
evalCache :: forall v syntax ann
          . ( Ord v
            , Ord (LocationFor v)
            , Ord ann
            , Ord (Cell (LocationFor v) v)
            , Ord1 syntax
            , Foldable (Cell (LocationFor v))
            , FreeVariables1 syntax
            , Functor syntax
            , MonadAddress (LocationFor v) (Eff (CachingInterpreter (Term syntax ann) v))
            , Semigroup (Cell (LocationFor v) v)
            , ValueRoots (LocationFor v) v
            , Eval (Term syntax ann) v (Eff (CachingInterpreter (Term syntax ann) v)) syntax
            )
          => Term syntax ann
          -> CachingResult (Term syntax ann) v
evalCache e = run @(CachingInterpreter (Term syntax ann) v) (fixCache (fix (evCache (evCollect (evRoots)))) pure e)


evCache :: forall t v m
        . ( Ord (LocationFor v)
          , Ord t
          , Ord v
          , Ord (Cell (LocationFor v) v)
          , MonadCachingInterpreter t v m
          )
        => (Eval' t m v -> Eval' t m v)
        -> Eval' t m v
        -> Eval' t m v
evCache ev0 ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e (Set.toList roots) env store :: Configuration (LocationFor v) t v
  out <- getCache
  case cacheLookup c out of
    Just pairs -> asum . flip map (toList pairs) $ \ (value, store') -> do
      putStore store'
      return value
    Nothing -> do
      in' <- askCache
      let pairs = fromMaybe mempty (cacheLookup c in')
      putCache (cacheSet c pairs out)
      v <- ev0 ev' yield e
      store' <- getStore
      modifyCache (cacheInsert c (v, store'))
      return v

fixCache :: forall t v m
         . ( Ord (LocationFor v)
           , Ord t
           , Ord v
           , Ord (Cell (LocationFor v) v)
           , MonadCachingInterpreter t v m
           , MonadNonDet m
           , MonadFresh m
           )
         => Eval' t m v
         -> Eval' t m v
fixCache ev' yield e = do
  env <- askEnv
  store <- getStore
  roots <- askRoots
  let c = Configuration e (Set.toList roots) env store :: Configuration (LocationFor v) t v
  pairs <- mlfp mempty (\ dollar -> do
    putCache (mempty :: Cache (LocationFor v) t v)
    putStore store
    reset 0
    _ <- localCache (const dollar) (collect point (ev' yield e) :: m (Set.Set v))
    getCache)
  asum . flip map (maybe [] toList (cacheLookup c pairs)) $ \ (value, store') -> do
    putStore store'
    return value


mlfp :: (Eq a, Monad m) => a -> (a -> m a) -> m a
mlfp a f = loop a
  where loop x = do
          x' <- f x
          if x' == x then
            return x
          else
            loop x'
