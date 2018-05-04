{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadValues
( resumingBadValues
) where

import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Value (ValueError(..))
import Data.ByteString.Char8 (pack)
import Prologue

resumingBadValues :: (AbstractHole value, Member (State (Environment location value)) effects, Show value) => Evaluator location term value (Resumable (ValueError location value) ': effects) a -> Evaluator location term value effects a
resumingBadValues = raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ValueError" <> show err) *> case err of
  CallError val     -> yield val
  StringError val   -> yield (pack (show val))
  BoolError{}       -> yield True
  BoundsError{}     -> yield hole
  IndexError{}      -> yield hole
  NumericError{}    -> yield hole
  Numeric2Error{}   -> yield hole
  ComparisonError{} -> yield hole
  NamespaceError{}  -> lower getEnv >>= yield
  BitwiseError{}    -> yield hole
  Bitwise2Error{}   -> yield hole
  KeyValueError{}   -> yield (hole, hole)
  ArithmeticError{} -> yield hole))
