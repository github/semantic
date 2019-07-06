{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Interpose
( Interpose(..)
, interpose
, runInterpose
, InterposeC(..)
, Listener(..)
) where

import Control.Applicative
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum

data Interpose (eff :: (* -> *) -> * -> *) m k
  = forall a . Interpose (m a) (forall n x . eff n x -> m x) (a -> m k)

deriving instance Functor m => Functor (Interpose eff m)

instance HFunctor (Interpose eff) where
  hmap f (Interpose m h k) = Interpose (f m) (f . h) (f . k)

-- | Respond to requests for some specific effect with a handler.
--
--   The intercepted effects are not re-sent in the surrounding context; thus, the innermost nested 'interpose' listening for an effect will win, and the effect’s own handler will not get the chance to service the request.
--
--   Note that since 'Interpose' lacks an 'Effect' instance, only “pure” effects, i.e. effects which can be handled inside other effects using 'hmap' alone, can be run within the 'runInterpose' scope. This includes @Reader@, but not e.g. @State@ or @Error@.
interpose :: (Member (Interpose eff) sig, Carrier sig m)
          => m a
          -> (forall n x . eff n x -> m x)
          -> m a
interpose m f = send (Interpose m f pure)


-- | Run an 'Interpose' effect.
runInterpose :: InterposeC eff m a -> m a
runInterpose = runReader Nothing . runInterposeC

newtype InterposeC (eff :: (* -> *) -> * -> *) m a = InterposeC
  { runInterposeC :: ReaderC (Maybe (Listener eff (InterposeC eff m))) m a
  } deriving (Alternative, Applicative, Functor, Monad)

newtype Listener (eff :: (* -> *) -> * -> *) m = Listener (forall n x . eff n x -> m x)

-- -- TODO: Document the implementation of this, as it is extremely subtle.

-- runListener :: Listener eff (InterposeC eff m) -> eff (InterposeC eff m) (InterposeC eff m a) -> InterposeC eff m a
-- runListener (Listener listen) = listen

instance (Carrier sig m, Member eff sig) => Carrier (Interpose eff :+: sig) (InterposeC eff m) where
  eff = undefined
  -- eff (L (Interpose m h k)) =
  --   InterposeC (local (const (Just (Listener h))) (runInterposeC m)) >>= _ k
  -- eff (R other) = do
  --   listener <- InterposeC ask
  --   case (listener, prj other) of
  --     (Just listener, Just eff) -> runListener listener eff
  --     _                         -> InterposeC (eff (R (handleCoercible other)))
