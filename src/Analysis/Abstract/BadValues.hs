{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues
( BadValues
) where

import Control.Abstract.Analysis
import Data.Abstract.Value (ValueError(..))
import Data.ByteString.Char8 (pack)
import Prologue

newtype BadValues m (effects :: [* -> *]) a = BadValues { runBadValues :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (BadValues m)
deriving instance AnalyzeModule location term value inner outer m => AnalyzeModule location term value inner outer (BadValues m)
deriving instance AnalyzeTerm location term value inner outer m => AnalyzeTerm location term value inner outer (BadValues m)

instance ( AbstractHole value
         , Evaluator location term value m
         , Interpreter m effects
         , Member (State (Environment location value)) effects
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
