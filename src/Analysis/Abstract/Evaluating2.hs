{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating2 where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Linker
import Data.Abstract.FreeVariables
import Data.Abstract.Eval2
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
type Evaluating v
  = '[ Fail                                   -- For 'MonadFail'.
     , State  (Store (LocationFor v) v)       -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- Local environment
     , State  (Environment (LocationFor v) v) -- Global environment
     ]

-- | Evaluate a term to a value.
evaluate :: forall v term
         . ( Ord v
           , Ord (LocationFor v) -- For 'MonadStore'
           , Recursive term
           , Eval term v (Eff (Evaluating v)) (Base term)
           )
         => term
         -> Final (Evaluating v) v
evaluate = run @(Evaluating v) . fix (const step)
