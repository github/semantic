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
import Analysis.Name
import Control.Algebra
import Control.Monad ((>=>))
import Control.Monad.Fail as Fail
import Data.Text (Text)
import GHC.Generics (Generic1)

abstract :: Has (Domain term abstract) sig m => Intro term Name -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: Has (Domain term abstract) sig m => abstract -> m (Intro term Name)
concretize abstract = send (Concretize abstract pure)


unit :: forall term abstract m sig . Has (Domain term abstract) sig m => m abstract
unit = abstract @term A.Unit

bool :: forall term abstract m sig . Has (Domain term abstract) sig m => Bool -> m abstract
bool = abstract @term . A.Bool

asBool :: forall term abstract m sig . (Has (Domain term abstract) sig m, MonadFail m, forall a . Show a => Show (term a)) => abstract -> m Bool
asBool = concretize @term >=> \case
  A.Bool b -> pure b
  other    -> typeError "Bool" other

string :: forall term abstract m sig . Has (Domain term abstract) sig m => Text -> m abstract
string = abstract @term . A.String

asString :: forall term abstract m sig . (Has (Domain term abstract) sig m, MonadFail m, forall a . Show a => Show (term a)) => abstract -> m Text
asString = concretize @term >=> \case
  A.String t -> pure t
  other      -> typeError "String" other


data Domain term abstract m k
  = Abstract   (Intro term Name) (abstract        -> m k)
  | Concretize abstract          (Intro term Name -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term abstract)
instance Effect   (Domain term abstract)


typeError :: (Show a, MonadFail m) => String -> a -> m b
typeError expected actual = Fail.fail $ "expected " <> expected <> ", got " <> show actual
