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
, Domain(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Intro (Intro)
import qualified Analysis.Intro as A
import Control.Algebra
import Control.Monad ((>=>))
import Control.Monad.Fail as Fail
import Data.Text (Text)
import GHC.Generics (Generic1)

abstract :: Has (Domain term addr abstract) sig m => Intro term addr -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: Has (Domain term addr abstract) sig m => abstract -> m (Intro term addr)
concretize abstract = send (Concretize abstract pure)


unit :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => m abstract
unit = abstract @term @addr A.Unit

bool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Bool -> m abstract
bool = abstract @term @addr . A.Bool

asBool :: forall term addr abstract m sig . (Has (Domain term addr abstract) sig m, MonadFail m, forall a . Show a => Show (term a), Show addr) => abstract -> m Bool
asBool = concretize @term @addr >=> \case
  A.Bool b -> pure b
  other    -> typeError "Bool" other

string :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Text -> m abstract
string = abstract @term @addr . A.String

asString :: forall term addr abstract m sig . (Has (Domain term addr abstract) sig m, MonadFail m, forall a . Show a => Show (term a), Show addr) => abstract -> m Text
asString = concretize @term @addr >=> \case
  A.String t -> pure t
  other      -> typeError "String" other


data Domain term addr abstract m k
  = Abstract   (Intro term addr) (abstract        -> m k)
  | Concretize abstract          (Intro term addr -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term addr abstract)
instance Effect   (Domain term addr abstract)


typeError :: (Show a, MonadFail m) => String -> a -> m b
typeError expected actual = Fail.fail $ "expected " <> expected <> ", got " <> show actual
