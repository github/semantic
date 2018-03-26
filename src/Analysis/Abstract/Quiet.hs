{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures #-}
module Analysis.Abstract.Quiet where

import Control.Abstract.Analysis
import Prologue

newtype Quiet m term value (effects :: [* -> *]) a = Quiet (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)
