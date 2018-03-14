{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Analysis.Abstract.Elaborating
( type Elaborating
) where

import Control.Abstract.Analysis
import Prologue

newtype Elaborating m term value (effects :: [* -> *]) a = Elaborating (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)
