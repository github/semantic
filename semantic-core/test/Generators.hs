{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

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

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Effect.Carrier
import qualified Core.Core as Core
import Core.Name

-- The 'prune' call here ensures that we don't spend all our time just generating
-- fresh names for variables, since the length of variable names is not an
-- interesting property as they parse regardless.
name :: MonadGen m => m (Named Name)
name = Gen.prune (named' <$> names) where
  names = Name <$> Gen.text (Range.linear 1 10) Gen.lower

boolean :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name)
boolean = Core.bool <$> Gen.bool

variable :: (Applicative t, MonadGen m) => m (t Name)
variable = pure . namedValue <$> name

ifthenelse :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name) -> m (t Name)
ifthenelse bod = Gen.subterm3 boolean bod bod Core.if'

apply :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name) -> m (t Name)
apply gen = go where
  go = Gen.recursive
    Gen.choice
    [ Gen.subterm2 gen gen (Core.$$)]
    [ Gen.subterm2 go go (Core.$$)  -- balanced
    , Gen.subtermM go (\x -> Core.lam <$> name <*> pure x)
    ]

lambda :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name) -> m (t Name)
lambda bod = do
  arg <- name
  Gen.subterm bod (Core.lam arg)

record :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name) -> m (t Name)
record bod = Core.record <$> Gen.list (Range.linear 0 5) ((,) . namedValue <$> name <*> bod)

atoms :: (Carrier sig t, Member Core.Core sig, MonadGen m) => [m (t Name)]
atoms = [variable, pure Core.unit, boolean, Core.string <$> Gen.text (Range.linear 1 10) Gen.lower]

literal :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name)
literal = Gen.recursive Gen.choice atoms [lambda literal, record literal]

expr :: (Carrier sig t, Member Core.Core sig, MonadGen m) => m (t Name)
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
  , Gen.subtermM expr (\ x -> (x Core..?)  . namedValue <$> name)
  , Gen.subterm2 expr expr (Core..=)
  ]
