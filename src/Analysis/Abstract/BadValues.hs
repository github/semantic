{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Value (ValueError(..))
import Data.ByteString.Char8 (pack)
import Prologue

newtype BadValues m (effects :: [* -> *]) a = BadValues { runBadValues :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadValues m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadValues m)
deriving instance Evaluator location term value m => Evaluator location term value (BadValues m)

instance ( Interpreter m effects
         , MonadEvaluator location term value effects m
         , AbstractHole value
         , Show value
         )
      => Interpreter (BadValues m) (Resumable (ValueError location value) ': effects) where
  type Result (BadValues m) (Resumable (ValueError location value) ': effects) result = Result m effects result
  interpret
    = interpret
    . runBadValues
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ValueError" <> show err) *> case err of
      CallError val     -> yield val
      StringError val   -> yield (pack (show val))
      BoolError{}       -> yield True
      BoundsError{}     -> yield hole
      IndexError{}      -> yield hole
      NumericError{}    -> yield hole
      Numeric2Error{}   -> yield hole
      ComparisonError{} -> yield hole
      NamespaceError{}  -> lower @m getEnv >>= yield
      BitwiseError{}    -> yield hole
      Bitwise2Error{}   -> yield hole
      KeyValueError{}   -> yield (hole, hole)
      ArithmeticError{} -> yield hole))
