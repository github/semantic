{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Analysis.Abstract.Erroring
( erroring
) where

import Control.Effect
import Control.Monad.Effect.Resumable

erroring :: forall exc m effects result . Effectful m => m (Resumable exc ': effects) result -> m effects (Either (SomeExc exc) result)
erroring = raiseHandler runError
