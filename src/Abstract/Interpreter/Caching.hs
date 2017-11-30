{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Caching where

import Abstract.Configuration
import Abstract.Environment
import Abstract.Eval
import Abstract.FreeVariables
import Abstract.Interpreter
import Abstract.Store
import Abstract.Value
import Control.Applicative
import Control.Effect
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Foldable
import Data.Function (fix)
import Data.Functor.Classes
import Data.Maybe
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Term

newtype Cache l t v = Cache { unCache :: Map.Map (Configuration l t v) (Set.Set (v, Store l v)) }

deriving instance (Eq l, Eq t, Eq v, Eq (Cell l v)) => Eq (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Ord (Cache l t v)
deriving instance (Show l, Show t, Show v, Show (Cell l v)) => Show (Cache l t v)
deriving instance (Ord l, Ord t, Ord v, Ord (Cell l v)) => Monoid (Cache l t v)

cacheLookup :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Cache l t v -> Maybe (Set.Set (v, Store l v))
cacheLookup key = Map.lookup key . unCache

cacheSet :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> Set.Set (v, Store l v) -> Cache l t v -> Cache l t v
cacheSet = (((Cache .) . (. unCache)) .) . Map.insert

cacheInsert :: (Ord l, Ord t, Ord v, Ord (Cell l v)) => Configuration l t v -> (v, Store l v) -> Cache l t v -> Cache l t v
cacheInsert = (((Cache .) . (. unCache)) .) . (. point) . Map.insertWith (<>)


type CachingInterpreter t v = '[Fresh, Reader (Set.Set (Address (LocationFor v) v)), Reader (Environment (LocationFor v) v), Fail, NonDetEff, State (Store (LocationFor v) v), Reader (Cache (LocationFor v) t v), State (Cache (LocationFor v) t v)]

type CachingResult t v = Final (CachingInterpreter t v) v

type MonadCachingInterpreter t v m = (MonadEnv v m, MonadStore v m, MonadCacheIn t v m, MonadCacheOut t v m, MonadGC v m, Alternative m)



class Monad m => MonadCacheIn t v m where
  askCache :: m (Cache (LocationFor v) t v)
  localCache :: (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> m a -> m a

instance (Reader (Cache (LocationFor v) t v) :< fs) => MonadCacheIn t v (Eff fs) where
  askCache = ask
  localCache = local


class Monad m => MonadCacheOut t v m where
  getCache :: m (Cache (LocationFor v) t v)
  putCache :: Cache (LocationFor v) t v -> m ()

instance (State (Cache (LocationFor v) t v) :< fs) => MonadCacheOut t v (Eff fs) where
  getCache = get
  putCache = put

modifyCache :: MonadCacheOut t v m => (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> m ()
modifyCache f = fmap f getCache >>= putCache


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


instance (Eq l, Eq1 (Cell l)) => Eq2 (Cache l) where
  liftEq2 eqT eqV (Cache a) (Cache b) = liftEq2 (liftEq2 eqT eqV) (liftEq (liftEq2 eqV (liftEq eqV))) a b

instance (Eq l, Eq t, Eq1 (Cell l)) => Eq1 (Cache l t) where
  liftEq = liftEq2 (==)


instance (Ord l, Ord1 (Cell l)) => Ord2 (Cache l) where
  liftCompare2 compareT compareV (Cache a) (Cache b) = liftCompare2 (liftCompare2 compareT compareV) (liftCompare (liftCompare2 compareV (liftCompare compareV))) a b

instance (Ord l, Ord t, Ord1 (Cell l)) => Ord1 (Cache l t) where
  liftCompare = liftCompare2 compare


instance (Show l, Show1 (Cell l)) => Show2 (Cache l) where
  liftShowsPrec2 spT slT spV slV d = showsUnaryWith (liftShowsPrec2 spKey slKey (liftShowsPrec spPair slPair) (liftShowList spPair slPair)) "Cache" d . unCache
    where spKey = liftShowsPrec2 spT slT spV slV
          slKey = liftShowList2 spT slT spV slV
          spPair = liftShowsPrec2 spV slV spStore slStore
          slPair = liftShowList2 spV slV spStore slStore
          spStore = liftShowsPrec spV slV
          slStore = liftShowList  spV slV

instance (Show l, Show t, Show1 (Cell l)) => Show1 (Cache l t) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
