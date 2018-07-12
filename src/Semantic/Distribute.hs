{-# LANGUAGE TypeOperators #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
) where

import qualified Control.Concurrent.Async as Async
import           Control.Parallel.Strategies
import           Control.Monad.Effect
import           Control.Monad.IO.Class
import           Prologue hiding (MonadError (..))

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member Distribute effs, Traversable t) => t (Eff effs output) -> Eff effs (t output)
distribute = fmap (withStrategy (parTraversable rseq)) <$> traverse (send . Distribute)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Member Distribute effs, Traversable t) => t a -> (a -> Eff effs output) -> Eff effs (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Member Distribute effs, Monoid output, Traversable t) => (a -> Eff effs output) -> t a -> Eff effs output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))


-- | Distribute effects run tasks concurrently.
newtype Distribute task output = Distribute (task output)

instance Effect Distribute where
  handleState c dist (Distribute task) k = Request (Distribute (dist (task <$ c) k)) pure


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: Eff '[Distribute, Lift IO] a -> Eff '[Lift IO] a
runDistribute = interpret (\ (Distribute task) -> liftIO (Async.runConcurrently (Async.Concurrently (runM (runDistribute task)))))
