{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, LambdaCase #-}
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

import Analysis.Intro as A
import Control.Algebra
import Control.Monad ((>=>))
import Control.Monad.Fail as Fail
import Data.Text (Text)
import GHC.Generics (Generic1)

abstract :: Has (Domain abstract) sig m => Intro -> m abstract
abstract concrete = send (Abstract concrete pure)

concretize :: Has (Domain abstract) sig m => abstract -> m Intro
concretize abstract = send (Concretize abstract pure)


unit :: Has (Domain abstract) sig m => m abstract
unit = abstract A.Unit

bool :: Has (Domain abstract) sig m => Bool -> m abstract
bool = abstract . A.Bool

asBool :: (Has (Domain abstract) sig m, MonadFail m) => abstract -> m Bool
asBool = concretize >=> \case
  A.Bool b -> pure b
  other    -> typeError "Bool" other

string :: Has (Domain abstract) sig m => Text -> m abstract
string = abstract . A.String

asString :: (Has (Domain abstract) sig m, MonadFail m) => abstract -> m Text
asString = concretize >=> \case
  A.String t -> pure t
  other      -> typeError "String" other


data Domain abstract m k
  = Abstract   Intro    (abstract -> m k)
  | Concretize abstract (Intro    -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain abstract)
instance Effect   (Domain abstract)


typeError :: (Show a, MonadFail m) => String -> a -> m b
typeError expected actual = Fail.fail $ "expected " <> expected <> ", got " <> show actual
