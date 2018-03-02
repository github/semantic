{-# LANGUAGE DataKinds #-}
module Analysis.Abstract.CallGraph where

import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
import Data.Abstract.Value

type CallGraphEffects term value
  = '[ Fail
     , NonDetEff
     , State  (StoreFor value)
     , State  (EnvironmentFor value)
     , Reader (EnvironmentFor value)
     , Reader (Linker term)
     , State  (Linker value)
     ]
