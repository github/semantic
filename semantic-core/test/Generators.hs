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
import Data.Term

-- The 'prune' call here ensures that we don't spend all our time just generating
-- fresh names for variables, since the length of variable names is not an
-- interesting property as they parse regardless.
name :: MonadGen m => m (Named User)
name = Gen.prune ((Named . Ignored <*> id) <$> names) where
  names = Gen.text (Range.linear 1 10) Gen.lower

boolean :: MonadGen m => m (Term Core User)
boolean = bool <$> Gen.bool

variable :: MonadGen m => m (Term Core User)
variable = pure . namedValue <$> name

ifthenelse :: MonadGen m => m (Term Core User) -> m (Term Core User)
ifthenelse bod = Gen.subterm3 boolean bod bod if'

apply :: MonadGen m => m (Term Core User) -> m (Term Core User)
apply gen = go where
  go = Gen.recursive
    Gen.choice
    [ Gen.subterm2 gen gen ($$)]
    [ Gen.subterm2 go go ($$)  -- balanced
    , Gen.subtermM go (\x -> lam <$> name <*> pure x)
    ]

lambda :: MonadGen m => m (Term Core User) -> m (Term Core User)
lambda bod = do
  arg <- name
  Gen.subterm bod (lam arg)

atoms :: MonadGen m => [m (Term Core User)]
atoms = [boolean, variable, pure unit, pure frame]

literal :: MonadGen m => m (Term Core User)
literal = Gen.recursive Gen.choice atoms [lambda literal]
