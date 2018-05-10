{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
, Action(..)
) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Effect hiding (run)
import           Control.Monad.Effect.Exception
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Prologue hiding (MonadError (..))

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member (Distribute task) effs, Traversable t) => t (task output) -> Eff effs (t output)
distribute = send . Distribute

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Member (Distribute task) effs, Traversable t) => t a -> (a -> task output) -> Eff effs (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Member (Distribute task) effs, Monoid output, Traversable t) => (a -> task output) -> t a -> Eff effs output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))


-- | Distribute effects run tasks concurrently.
data Distribute task output where
  Distribute :: Traversable t => t (task output) -> Distribute task (t output)


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: Members '[Exc SomeException, IO] effs => Eff (Distribute task ': effs) a -> Action task -> Eff effs a
runDistribute m action = interpret (\ (Distribute tasks) ->
  liftIO (Async.mapConcurrently (runAction action) tasks) >>= either throwError pure . sequenceA . withStrategy (parTraversable (parTraversable rseq))) m


-- | An action evaluating @task@s to some output in 'IO', or failing with an exception.
--
--   This is necessary because GHC won’t allow us to use a rank-n quantified type in the third parameter to our instance of 'Run', below.
newtype Action task = Action { runAction :: forall output . task output -> IO (Either SomeException output) }
