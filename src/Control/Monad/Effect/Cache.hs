{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Cache where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Cache
import Data.Abstract.Value

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
