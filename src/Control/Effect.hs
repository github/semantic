{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Effect
( Effectful(..)
-- * Effects
, Eff.Reader
, Eff.State
, Fresh
, Trace
, send
, throwResumable
, traceE
-- * Handlers
, run
, runM
, runEffect
, reinterpretEffect
, raiseHandler
, runReader
, runState
, runFresh
, resume
, runResumableWith
, runIgnoringTraces
, runPrintingTraces
, runReturningTraces
) where

import qualified Control.Monad.Effect as Eff
import           Control.Monad.Effect.Fresh
import qualified Control.Monad.Effect.Reader as Eff
import           Control.Monad.Effect.Resumable
import qualified Control.Monad.Effect.State as Eff
import           Control.Monad.Effect.Trace
import           Prologue hiding (throwError)

-- | Types wrapping 'Eff.Eff' actions.
--
--   Most instances of 'Effectful' will be derived using @-XGeneralizedNewtypeDeriving@, with these ultimately bottoming out on the instance for 'Eff.Eff' (for which 'raise' and 'lower' are simply the identity). Because of this, types can be nested arbitrarily deeply and still call 'raise'/'lower' just once to get at the (ultimately) underlying 'Eff.Eff'.
class Effectful m where
  -- | Raise an action in 'Eff' into an action in @m@.
  raise :: Eff.Eff effects a -> m effects a
  -- | Lower an action in @m@ into an action in 'Eff'.
  lower :: m effects a -> Eff.Eff effects a

instance Effectful Eff.Eff where
  raise = id
  lower = id


-- Effects

send :: (Effectful m, Member effect effects) => effect result -> m effects result
send = raise . Eff.send

throwResumable :: (Member (Resumable exc) effects, Effectful m) => exc v -> m effects v
throwResumable = raise . throwError

-- | Trace into the current context.
traceE :: (Effectful m, Member Trace effects) => String -> m effects ()
traceE = raise . trace


-- Handlers

run :: Effectful m => m '[] a -> a
run = Eff.run . lower

runM :: (Effectful m, Monad f) => m '[f] a -> f a
runM = Eff.runM . lower

runEffect :: Effectful m => (forall v . effect v -> (v -> m effects a) -> m effects a) -> m (effect ': effects) a -> m effects a
runEffect handler = raiseHandler (Eff.relay pure (\ effect yield -> lower (handler effect (raise . yield))))

reinterpretEffect :: Effectful m => (forall x . effect x -> m (newEffect ': effects) x) -> m (effect ': effects) a -> m (newEffect ': effects) a
reinterpretEffect handler = raiseHandler (Eff.reinterpret (lower . handler))

-- | Raise a handler on 'Eff.Eff' to a handler on some 'Effectful' @m@.
raiseHandler :: Effectful m => (Eff.Eff effectsA a -> Eff.Eff effectsB b) -> m effectsA a -> m effectsB b
raiseHandler handler = raise . handler . lower

-- | Run a 'Reader' effect in an 'Effectful' context.
runReader :: Effectful m => info -> m (Eff.Reader info ': effects) a -> m effects a
runReader = raiseHandler . flip Eff.runReader

-- | Run a 'State' effect in an 'Effectful' context.
runState :: Effectful m => state -> m (Eff.State state ': effects) a -> m effects (a, state)
runState = raiseHandler . flip Eff.runState

-- | Run a 'Fresh' effect in an 'Effectful' context.
runFresh :: Effectful m => Int -> m (Fresh ': effects) a -> m effects a
runFresh = raiseHandler . flip runFresh'

resume :: (Member (Resumable exc) effects, Effectful m) => m effects a -> (forall v . exc v -> m effects v) -> m effects a
resume m handle = raise (resumeError (lower m) (\yield -> yield <=< lower . handle))

-- | Run a 'Resumable' effect in an 'Effectful' context, using a handler to resume computation.
runResumableWith :: Effectful m => (forall resume . exc resume -> m effects resume) -> m (Resumable exc ': effects) a -> m effects a
runResumableWith handler = raiseHandler (Eff.relay pure (\ (Resumable err) -> (lower (handler err) >>=)))

-- | Run a 'Trace' effect, discarding the traced values.
runIgnoringTraces :: Effectful m => m (Trace ': effects) a -> m effects a
runIgnoringTraces = runEffect (\(Trace _) yield -> yield ())

-- | Run a 'Trace' effect, printing the traced values to stdout.
runPrintingTraces :: (Member IO effects, Effectful m) => m (Trace ': effects) a -> m effects a
runPrintingTraces = raiseHandler (Eff.relay pure (\(Trace s) yield -> Eff.send (putStrLn s) >>= yield))

-- | Run a 'Trace' effect, accumulating the traced values into a list.
runReturningTraces :: (Functor (m effects), Effectful m) => m (Trace ': effects) a -> m effects (a, [String])
runReturningTraces e = fmap reverse <$> raiseHandler (Eff.relayState [] (\ts a -> pure (a, ts)) (\ts (Trace s) yield -> yield (s : ts) ())) e
