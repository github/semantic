module Language.Python.Generators
  ( boolean
  ) where

import Data.Bool
import qualified TreeSitter.Python.AST as Py

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Shrinks to @False@.
boolean :: MonadGen m => m Py.PrimaryExpression
boolean = bool false true <$> Gen.bool where
  false = Py.FalsePrimaryExpression (Py.False "false")
  true  = Py.TruePrimaryExpression (Py.True "true")
