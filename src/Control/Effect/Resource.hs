{-# LANGUAGE DeriveFunctor, ExistentialQuantification, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resource
( Resource(..)
, bracket
, runResource
, ResourceC(..)
) where

import           Control.Effect hiding (bracket)
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class

data Resource m k
  = forall resource any output . Resource (m resource) (resource -> m any) (resource -> m output) (output -> k)

deriving instance Functor (Resource m)

instance HFunctor Resource where
  hmap f (Resource acquire release use k) = Resource (f acquire) (f . release) (f . use) k

instance Effect Resource where
  handle state handler (Resource acquire release use k) = Resource (handler (acquire <$ state)) (handler . fmap release) (handler . fmap use) (handler . fmap k)

bracket :: (Member Resource sig, Carrier sig m)
        => m resource
        -> (resource -> m any)
        -> (resource -> m a)
        -> m a
bracket acquire release use = send (Resource acquire release use gen)


runResource :: (Carrier sig m, MonadIO m)
            => (forall x . m x -> IO x)
            -> Eff (ResourceC m) a
            -> m a
runResource handler = runResourceC handler . interpret

newtype ResourceC m a = ResourceC ((forall x . m x -> IO x) -> m a)

runResourceC :: (forall x . m x -> IO x) -> ResourceC m a -> m a
runResourceC handler (ResourceC m) = m handler

instance (Carrier sig m, MonadIO m) => Carrier (Resource :+: sig) (ResourceC m) where
  gen a = ResourceC (const (gen a))
  alg op = ResourceC (\ handler -> (algR handler \/ alg . handlePure (runResourceC handler)) op)
    where algR :: MonadIO m => (forall x . m x -> IO x) -> Resource (ResourceC m) (ResourceC m a) -> m a
          algR handler (Resource acquire release use k) = liftIO (Exc.bracket
            (handler (runResourceC handler acquire))
            (handler . runResourceC handler . release)
            (handler . runResourceC handler . use))
            >>= runResourceC handler . k
