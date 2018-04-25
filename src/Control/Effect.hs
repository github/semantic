{-# LANGUAGE FunctionalDependencies, RankNTypes #-}
module Control.Effect
( Effectful(..)
, Interpreter(..)
, resume
) where

import Control.Monad.Effect           as Effect
import Control.Monad.Effect.Resumable as Resumable

resume :: (Member (Resumable exc) e, Effectful m) => m e a -> (forall v . (v -> m e a) -> exc v -> m e a) -> m e a
resume m handle = raise (resumeError (lower m) (\yield -> lower . handle (raise . yield)))


-- | Types wrapping 'Eff' actions.
--
--   Most instances of 'Effectful' will be derived using @-XGeneralizedNewtypeDeriving@, with these ultimately bottoming out on the instance for 'Eff' (for which 'raise' and 'lower' are simply the identity). Because of this, types can be nested arbitrarily deeply and still call 'raise'/'lower' just once to get at the (ultimately) underlying 'Eff'.
class Effectful m where
  -- | Raise an action in 'Eff' into an action in @m@.
  raise :: Eff effects a -> m effects a
  -- | Lower an action in @m@ into an action in 'Eff'.
  lower :: m effects a -> Eff effects a

instance Effectful Eff where
  raise = id
  lower = id


class Effectful m => Interpreter effects result function m | m -> effects, m result -> function where
  interpret :: m effects result -> function

instance Interpreter '[] result result Eff where
  interpret = Effect.run
