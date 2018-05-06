{-# LANGUAGE FunctionalDependencies, RankNTypes, TypeFamilies, TypeOperators #-}
module Control.Effect
( Effectful(..)
, Interpreter(..)
, throwResumable
, resume
-- * Effects
, Reader
, State
-- * Handlers
, raiseHandler
, handleReader
, handleState
) where

import Control.Monad.Effect.Internal
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Resumable as Resumable
import Control.Monad.Effect.State

throwResumable :: (Member (Resumable exc) effects, Effectful m) => exc v -> m effects v
throwResumable = raise . throwError

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


-- | Interpreters determine and interpret a list of effects, optionally taking extra arguments.
--
--   Instances will generally be defined recursively in terms of underlying interpreters, bottoming out with the instance for 'Eff' which uses 'run' to produce a final value.
class Effectful m => Interpreter m effects | m -> effects where
  type Result m effects result
  type instance Result m effects result = result
  interpret :: m effects result -> Result m effects result

instance Interpreter Eff '[] where
  interpret = run


-- Handlers

-- | Raise a handler on 'Eff' to a handler on some 'Effectful' @m@.
raiseHandler :: Effectful m => (Eff effectsA a -> Eff effectsB b) -> m effectsA a -> m effectsB b
raiseHandler handler = raise . handler . lower

-- | Run a 'Reader' effect in an 'Effectful' context.
handleReader :: Effectful m => info -> m (Reader info ': effects) a -> m effects a
handleReader = raiseHandler . flip runReader

-- | Run a 'State' effect in an 'Effectful' context.
handleState :: Effectful m => state -> m (State state ': effects) a -> m effects (a, state)
handleState = raiseHandler . flip runState
