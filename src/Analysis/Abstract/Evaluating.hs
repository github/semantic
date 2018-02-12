{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications #-}
module Analysis.Abstract.Evaluating where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Eval
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Semigroup

-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail                                   -- For 'MonadFail'.
     , State (Store (LocationFor v) v)        -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- For 'MonadEnv'.
     , Reader (Live (LocationFor v) v)        -- For 'MonadGC'.
     ]

-- | Evaluate a term to a value.
evaluate :: forall v term
         . ( Ord v
           , Ord (Cell (LocationFor v) v)
           , Semigroup (Cell (LocationFor v) v)
           , Functor (Base term)
           , Recursive term
           , MonadAddress (LocationFor v) (Eff (Evaluating v))
           , Eval term v (Eff (Evaluating v)) (Base term)
           )
         => term
         -> Final (Evaluating v) v
evaluate = run @(Evaluating v) . fix go pure
  where go recur yield = eval recur yield . project
