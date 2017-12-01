{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Analysis.Abstract.Dead where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Dead
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Pointed
import Data.Semigroup
import Data.Set


type DeadCodeInterpreter t v = '[State (Dead t), Fail, State (Store (LocationFor v) v), Reader (Set (Address (LocationFor v) v)), Reader (Environment (LocationFor v) v)]

type DeadCodeResult t v = Final (DeadCodeInterpreter t v) v


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set a
subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term


-- Dead code analysis
--
-- Example:
--    evalDead @(Value Syntax Precise) <term>

evalDead :: forall v term
         . ( Ord v
           , Ord term
           , Foldable (Base term)
           , Recursive term
           , Eval term v (Eff (DeadCodeInterpreter term v)) (Base term)
           , MonadAddress (LocationFor v) (Eff (DeadCodeInterpreter term v))
           , Semigroup (Cell (LocationFor v) v)
           )
         => term
         -> DeadCodeResult term v
evalDead e0 = run @(DeadCodeInterpreter term v) $ do
  killAll (Dead (subterms e0))
  fix (evDead (\ recur yield -> eval recur yield . project)) pure e0

evDead :: (Ord t, MonadDead t m)
       => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
       -> ((v -> m v) -> t -> m v)
       -> (v -> m v) -> t -> m v
evDead ev0 ev' yield e = do
  revive e
  ev0 ev' yield e
