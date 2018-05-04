{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Erroring
( erroring
) where

import Control.Effect
import Control.Monad.Effect.Resumable

erroring :: Effectful m => m (Resumable exc ': effects) result -> m effects (Either (SomeExc exc) result)
erroring = raiseHandler runError
