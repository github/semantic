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
name :: MonadGen m => m (Named Name)
name = Gen.prune ((Named . Ignored <*> User) <$> names) where
  names = Gen.text (Range.linear 1 10) Gen.lower

boolean :: MonadGen m => m (Term Core Name)
boolean = bool <$> Gen.bool

variable :: MonadGen m => m (Term Core Name)
variable = pure . namedValue <$> name

ifthenelse :: MonadGen m => m (Term Core Name) -> m (Term Core Name)
ifthenelse bod = Gen.subterm3 boolean bod bod if'

apply :: MonadGen m => m (Term Core Name) -> m (Term Core Name)
apply gen = go where
  go = Gen.recursive
    Gen.choice
    [ Gen.subterm2 gen gen ($$)]
    [ Gen.subterm2 go go ($$)  -- balanced
    , Gen.subtermM go (\x -> lam <$> name <*> pure x)
    ]

lambda :: MonadGen m => m (Term Core Name) -> m (Term Core Name)
lambda bod = do
  arg <- name
  Gen.subterm bod (lam arg)

atoms :: MonadGen m => [m (Term Core Name)]
atoms = [boolean, variable, pure unit, pure frame]

literal :: MonadGen m => m (Term Core Name)
literal = Gen.recursive Gen.choice atoms [lambda literal]
