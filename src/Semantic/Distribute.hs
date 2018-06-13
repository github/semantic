{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Effect
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Prologue hiding (MonadError (..))

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member Distribute effs, Traversable t) => t (Eff effs output) -> Eff effs (t output)
distribute = send . Distribute

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
data Distribute m output where
  Distribute :: Traversable t => t (m output) -> Distribute m (t output)


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: Eff '[Distribute, Lift IO] a -> Eff '[Lift IO] a
runDistribute = interpret $ \ (Distribute tasks) ->
  liftIO (Async.mapConcurrently (runM . runDistribute) tasks) >>= pure . withStrategy (parTraversable rseq)
