{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating3 where

import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Store2
import Control.Monad.Effect.State
import Control.Monad.Effect.Reader
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
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)
     , Reader (EnvironmentFor v)
     ]

-- | Evaluate a term to a value.
evaluate :: forall v term.
          ( Ord v
          , Ord (LocationFor v) -- For 'MonadStore'
          , Recursive term
          , Evaluatable (Evaluating term v) term v (Base term)
          )
         => term
         -> Final (Evaluating term v) v
evaluate = run @(Evaluating term v) . fix (const step)
