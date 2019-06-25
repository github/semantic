{-# LANGUAGE ScopedTypeVariables #-}

module Generators
  ( literal
  , name
  , variable
  , boolean
  , lambda
  , apply
  , ifthenelse
  ) where

import Prelude hiding (span)

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Core
import Data.Name

-- The 'prune' call here ensures that we don't spend all our time just generating
-- fresh names for variables, since the length of variable names is not an
-- interesting property as they parse regardless.
name :: MonadGen m => m Name
name = Gen.prune (User <$> names) where
  names = Gen.text (Range.linear 1 10) Gen.lower

boolean :: MonadGen m => m Core
boolean = Bool <$> Gen.bool

variable :: MonadGen m => m Core
variable = Var <$> name

ifthenelse :: MonadGen m => m Core -> m Core
ifthenelse bod = Gen.subterm3 boolean bod bod If

apply :: MonadGen m => m Core -> m Core
apply gen = go where
  go = Gen.recursive
    Gen.choice
    [ Gen.subterm2 gen gen (:$)]
    [ Gen.subterm2 go go (:$)  -- balanced
    , Gen.subtermM go (\x -> Lam <$> name <*> pure x)
    ]

lambda :: MonadGen m => m Core -> m Core
lambda bod = do
  arg <- name
  Gen.subterm bod (Lam arg)

atoms :: MonadGen m => [m Core]
atoms = [boolean, variable, pure Unit, pure Frame]

literal :: MonadGen m => m Core
literal = Gen.recursive Gen.choice atoms [lambda literal]
