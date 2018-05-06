{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadValues
( resumingBadValues
) where

import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Value (ValueError(..), runValueErrorWith)
import Data.ByteString.Char8 (pack)
import Prologue

resumingBadValues :: (AbstractHole value, Member (State (Environment location value)) effects, Show value) => Evaluator location term value (Resumable (ValueError location value) ': effects) a -> Evaluator location term value effects a
resumingBadValues = runValueErrorWith (\ err -> traceM ("ValueError" <> show err) *> case err of
  CallError val     -> pure val
  StringError val   -> pure (pack (show val))
  BoolError{}       -> pure True
  BoundsError{}     -> pure hole
  IndexError{}      -> pure hole
  NumericError{}    -> pure hole
  Numeric2Error{}   -> pure hole
  ComparisonError{} -> pure hole
  NamespaceError{}  -> getEnv
  BitwiseError{}    -> pure hole
  Bitwise2Error{}   -> pure hole
  KeyValueError{}   -> pure (hole, hole)
  ArithmeticError{} -> pure hole)
