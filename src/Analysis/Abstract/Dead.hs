{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}
module Analysis.Abstract.Dead where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Addressable
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

-- | The effects necessary for dead code analysis.
type DeadCodeEvaluating t v
  = '[ State (Dead t)                         -- For 'MonadDead'.
     , Fail                                   -- For 'MonadFail'.
     , State (Store (LocationFor v) v)        -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- For 'MonadEnv'.
     ]


-- | Dead code analysis
evalDead :: forall v term
         . ( Ord v
           , Ord term
           , Foldable (Base term)
           , Recursive term
           , Eval term v (Eff (DeadCodeEvaluating term v)) (Base term)
           , Addressable (LocationFor v) (Eff (DeadCodeEvaluating term v))
           , Semigroup (Cell (LocationFor v) v)
           )
         => term
         -> Final (DeadCodeEvaluating term v) v
evalDead e0 = run @(DeadCodeEvaluating term v) $ do
  killAll (Dead (subterms e0))
  fix (evDead (\ recur yield -> eval recur yield . project)) pure e0
  where
    subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set a
    subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term

-- | Evaluation which 'revive's each visited term.
evDead :: (Ord t, MonadDead t m)
       => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
       -> ((v -> m v) -> t -> m v)
       -> (v -> m v) -> t -> m v
evDead ev0 ev' yield e = do
  revive e
  ev0 ev' yield e
