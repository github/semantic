{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Effect
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
runDistribute :: (Member (Exc SomeException) effs, Member (Lift IO) effs, Effects effs) => (forall output . task output -> IO (Either SomeException output)) -> Eff (Distribute task ': effs) a -> Eff effs a
runDistribute action = interpret (\ (Distribute tasks) ->
  liftIO (Async.mapConcurrently action tasks) >>= either throwError pure . sequenceA . withStrategy (parTraversable (parTraversable rseq)))
