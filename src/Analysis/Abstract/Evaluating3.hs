{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating3 where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Store2
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Linker
import Data.Abstract.FreeVariables
import Data.Abstract.Eval3
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Abstract.Live
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)
import Data.Blob
import System.FilePath.Posix

-- | The effects necessary for concrete interpretation.
type Evaluating term v
  = '[ Fail                                   -- For 'MonadFail'.
     , Store2 v       -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- Local environment
     , State  (Environment (LocationFor v) v) -- Global environment
     -- , Eval (Base term) term
     -- , EvalEnv
     ]


-- | Evaluate a term to a value.
evaluate :: forall term v
         . ( Ord v
           , Ord (LocationFor v) -- For 'MonadStore'
           , Recursive term
           , Evaluatable (Base term) term v
           )
         => term
         -> Final (Evaluating term v) v
evaluate = run @(Evaluating term v) .
  runEval .
  runEvalEnv .
  fix (const eval)
