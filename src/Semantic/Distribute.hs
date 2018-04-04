{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Effect hiding (run)
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Run
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Prologue hiding (MonadError (..))

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member (Distribute task) effs, Traversable t) => t (task output) -> Eff effs (t output)
distribute = send . Distribute

-- | Distribute a 'Bitraversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'bisequenceA'.
bidistribute :: (Bitraversable t, Member (Distribute task) effs) => t (task output1) (task output2) -> Eff effs (t output1 output2)
bidistribute = send . Bidistribute

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Member (Distribute task) effs, Traversable t) => t a -> (a -> task output) -> Eff effs (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Bitraversable' container of inputs over the available cores (i.e. perform the functions concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'bifor' or 'bitraverse' (with the arguments flipped).
bidistributeFor :: (Bitraversable t, Member (Distribute task) effs) => t a b -> (a -> task output1) -> (b -> task output2) -> Eff effs (t output1 output2)
bidistributeFor inputs toTask1 toTask2 = bidistribute (bimap toTask1 toTask2 inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Member (Distribute task) effs, Monoid output, Traversable t) => (a -> task output) -> t a -> Eff effs output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))


data Distribute task output where
  Distribute   :: Traversable t   => t (task output)                 -> Distribute task (t output)
  Bidistribute :: Bitraversable t => t (task output1) (task output2) -> Distribute task (t output1 output2)


runDistribute :: Members '[Exc SomeException, IO] effs => Eff (Distribute task ': effs) a -> Action task -> Eff effs a
runDistribute m action = interpret (\ task -> case task of
  Distribute tasks -> liftIO (Async.mapConcurrently (runAction action) tasks) >>= either throwError pure . sequenceA . withStrategy (parTraversable (parTraversable rseq))
  Bidistribute tasks -> liftIO (Async.runConcurrently (bitraverse (Async.Concurrently . runAction action) (Async.Concurrently . runAction action) tasks)) >>= either throwError pure . bisequenceA . withStrategy (parBitraversable (parTraversable rseq) (parTraversable rseq))) m

parBitraversable :: Bitraversable t => Strategy a -> Strategy b -> Strategy (t a b)
parBitraversable strat1 strat2 = bitraverse (rparWith strat1) (rparWith strat2)


newtype Action task = Action { runAction :: forall output . task output -> IO (Either SomeException output) }

instance (Members '[Exc SomeException, IO] effects, Run effects result rest) => Run (Distribute task ': effects) result (Action task -> rest) where
  run = fmap run . runDistribute
