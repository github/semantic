{-# LANGUAGE ExistentialQuantification, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Interpose
( Interpose(..)
, interpose
, runInterpose
, InterposeC(..)
, Listener(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum

data Interpose eff m k
  = forall a . Interpose (m a) (forall n x . eff n (n x) -> m x) (a -> k)

deriving instance Functor (Interpose eff m)

instance HFunctor (Interpose eff) where
  hmap f (Interpose m h k) = Interpose (f m) (f . h) k

-- | Respond to requests for some specific effect with a handler.
--
--   The intercepted effects are not re-sent in the surrounding context; thus, the innermost nested 'interpose' listening for an effect will win, and the effect’s own handler will not get the chance to service the request.
--
--   Note that since 'Interpose' lacks an 'Effect' instance, only “pure” effects, i.e. effects which can be handled inside other effects using 'hmap' alone, can be run within the 'runInterpose' scope. This includes @Reader@, but not e.g. @State@ or @Error@.
interpose :: (Member (Interpose eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n (n x) -> m x)
          -> m a
interpose m f = send (Interpose m f ret)


-- | Run an 'Interpose' effect.
runInterpose :: (Member eff sig, Carrier sig m, Monad m) => Eff (InterposeC eff m) a -> m a
runInterpose = flip runInterposeC Nothing . interpret

newtype InterposeC eff m a = InterposeC { runInterposeC :: Maybe (Listener eff m) -> m a }

newtype Listener eff m = Listener { runListener :: forall n x . eff n (n x) -> m x }

instance (Carrier sig m, Member eff sig, Monad m) => Carrier (Interpose eff :+: sig) (InterposeC eff m) where
  ret a = InterposeC (const (ret a))
  eff op = InterposeC (\ listener -> handleSum (algOther listener) (alg listener) op)
    where alg listener (Interpose m h k) = runInterposeC m (Just (Listener (flip runInterposeC listener . h))) >>= flip runInterposeC listener . k
          algOther listener op
            | Just listener <- listener
            , Just eff <- prj op = runListener listener eff
            | otherwise          = eff (handleReader listener runInterposeC op)
