{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, RankNTypes, TypeFamilies, TypeOperators #-}
module Control.Effect
( Effectful(..)
, raiseHandler
, Interpreter(..)
, Evaluator(..)
, throwResumable
, resume
, throwException
, catchException
) where

import Control.Monad.Effect           as Effect
import Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Resumable as Resumable

throwResumable :: (Member (Resumable exc) effects, Effectful m) => exc v -> m effects v
throwResumable = raise . Resumable.throwError

resume :: (Member (Resumable exc) e, Effectful m) => m e a -> (forall v . (v -> m e a) -> exc v -> m e a) -> m e a
resume m handle = raise (resumeError (lower m) (\yield -> lower . handle (raise . yield)))

throwException :: (Member (Exc exc) effects, Effectful m) => exc -> m effects a
throwException = raise . Exception.throwError

catchException :: (Member (Exc exc) effects, Effectful m) => m effects v -> (exc -> m effects v) -> m effects v
catchException action handler = raise (lower action `Exception.catchError` (lower . handler))


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

raiseHandler :: Effectful m => (Eff effectsA a -> Eff effectsB b) -> m effectsA a -> m effectsB b
raiseHandler handler = raise . handler . lower


-- | Interpreters determine and interpret a list of effects, optionally taking extra arguments.
--
--   Instances will generally be defined recursively in terms of underlying interpreters, bottoming out with the instance for 'Eff' which uses 'Effect.run' to produce a final value.
class Effectful m => Interpreter effects m | m -> effects where
  type Result effects m result
  type instance Result effects m result = result
  interpret :: m effects result -> Result effects m result

instance Interpreter '[] Eff where
  interpret = Effect.run


data Evaluator m a = forall effects . Evaluator { runEvaluator :: Interpreter effects m => m effects a }
