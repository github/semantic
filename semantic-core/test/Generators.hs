{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Generators
  ( literal
  , name
  , variable
  , boolean
  , lambda
  , record
  , apply
  , ifthenelse
  , expr
  ) where

import Prelude hiding (span)

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Effect.Sum ((:+:))
import qualified Data.Core as Core
import Data.Name
import Data.Term

-- The 'prune' call here ensures that we don't spend all our time just generating
-- fresh names for variables, since the length of variable names is not an
-- interesting property as they parse regardless.
name :: MonadGen m => m (Named User)
name = Gen.prune (named' <$> names) where
  names = Gen.text (Range.linear 1 10) Gen.lower

boolean :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User)
boolean = Core.bool <$> Gen.bool

variable :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User)
variable = pure . namedValue <$> name

ifthenelse :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User) -> m (Term (Core.Ann :+: Core.Core) User)
ifthenelse bod = Gen.subterm3 boolean bod bod Core.if'

apply :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User) -> m (Term (Core.Ann :+: Core.Core) User)
apply gen = go where
  go = Gen.recursive
    Gen.choice
    [ Gen.subterm2 gen gen (Core.$$)]
    [ Gen.subterm2 go go (Core.$$)  -- balanced
    , Gen.subtermM go (\x -> Core.lam <$> name <*> pure x)
    ]

lambda :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User) -> m (Term (Core.Ann :+: Core.Core) User)
lambda bod = do
  arg <- name
  Gen.subterm bod (Core.lam arg)

record :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User) -> m (Term (Core.Ann :+: Core.Core) User)
record bod = Core.record <$> Gen.list (Range.linear 0 5) ((,) . namedValue <$> name <*> bod)

atoms :: MonadGen m => [m (Term (Core.Ann :+: Core.Core) User)]
atoms = [variable, pure Core.unit, boolean, Core.string <$> Gen.text (Range.linear 1 10) Gen.lower]

literal :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User)
literal = Gen.recursive Gen.choice atoms [lambda literal, record literal]

expr :: MonadGen m => m (Term (Core.Ann :+: Core.Core) User)
expr = Gen.recursive Gen.choice atoms
  [ Gen.subtermM expr (\x -> flip Core.rec x <$> name)
  , Gen.subterm2 expr expr (Core.>>>)
  , Gen.subtermM2 expr expr (\ x y -> (Core.>>>= y) . (Core.:<- x) <$> name)
  , lambda expr
  , Gen.subterm2 expr expr (Core.$$)
  , Gen.subterm3 expr expr expr Core.if'
  , Gen.subterm expr Core.load
  , record expr
  , Gen.subtermM expr (\ x -> (x Core....) . namedValue <$> name)
  , Gen.subterm2 expr expr (Core..=)
  ]
