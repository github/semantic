{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Abstract.Analysis where

import Control.Abstract.Addressable
import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Prologue

class Monad m => MonadAnalysis term value m where
  evaluateTerm :: ( AbstractValue value
                  , FreeVariables term
                  , MonadAddressable (LocationFor value) value m
                  , Ord (LocationFor value)
                  , Semigroup (Cell (LocationFor value) value)
                  )
               => term
               -> m value
