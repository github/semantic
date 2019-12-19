{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, DeriveGeneric, FlexibleContexts, LambdaCase, QuantifiedConstraints, ScopedTypeVariables, TypeApplications #-}
module Analysis.Effect.Domain
( -- * Domain effect
  abstract
, concretize
, unit
, bool
, asBool
, string
, asString
, lam
, asLam
, record
, asRecord
, Domain(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Intro (Intro)
import qualified Analysis.Intro as A
import Analysis.Name
import Control.Algebra
import Control.Monad ((>=>))
import Control.Monad.Fail as Fail
import Data.Text (Text)
import GHC.Generics (Generic1)
import Syntax.Scope (Scope)

abstract :: Has (Domain term addr abstract) sig m => Intro term addr -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: Has (Domain term addr abstract) sig m => abstract -> m (Intro term addr)
concretize abstract = send (Concretize abstract pure)


unit :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => m abstract
unit = abstract @term @addr A.Unit

bool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Bool -> m abstract
bool = abstract @term @addr . A.Bool

asBool :: forall term addr abstract m sig . (Has (Domain term addr abstract) sig m, MonadFail m, Show addr, forall a . Show a => Show (term a)) => abstract -> m Bool
asBool = concretize @term @addr >=> \case
  A.Bool b -> pure b
  other    -> typeError "Bool" other

string :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Text -> m abstract
string = abstract @term @addr . A.String

asString :: forall term addr abstract m sig . (Has (Domain term addr abstract) sig m, MonadFail m, Show addr, forall a . Show a => Show (term a)) => abstract -> m Text
asString = concretize @term @addr >=> \case
  A.String t -> pure t
  other      -> typeError "String" other


lam :: Has (Domain term addr abstract) sig m => Named (Scope () term addr) -> m abstract
lam = abstract . A.Lam

asLam :: (Has (Domain term addr abstract) sig m, MonadFail m, Show addr, forall a . Show a => Show (term a)) => abstract -> m (Named (Scope () term addr))
asLam = concretize >=> \case
  A.Lam b -> pure b
  other   -> typeError "Lam" other


record :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => [(Name, term addr)] -> m abstract
record = abstract @term . A.Record

asRecord :: forall term addr abstract m sig . (Has (Domain term addr abstract) sig m, MonadFail m, Show addr, forall a . Show a => Show (term a)) => abstract -> m [(Name, term addr)]
asRecord = concretize @term >=> \case
  A.Record fs -> pure fs
  other       -> typeError "Record" other


data Domain term addr abstract m k
  = Abstract   (Intro term addr) (abstract        -> m k)
  | Concretize abstract          (Intro term addr -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term addr abstract)
instance Effect   (Domain term addr abstract)


typeError :: (Show a, MonadFail m) => String -> a -> m b
typeError expected actual = Fail.fail $ "expected " <> expected <> ", got " <> show actual
