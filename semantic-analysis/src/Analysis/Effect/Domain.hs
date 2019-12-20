{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Analysis.Effect.Domain
( -- * Domain effect
  abstract
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

import           Analysis.Intro (Intro)
import qualified Analysis.Intro as A
import           Analysis.Name
import           Control.Algebra
import           Data.Text (Text)
import           GHC.Generics (Generic1)
import           Syntax.Scope (Scope)

abstract :: Has (Domain term addr abstract) sig m => Intro term addr -> m abstract
abstract concrete = send (Abstract concrete pure)


unit :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => m abstract
unit = abstract @term @addr A.Unit

bool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Bool -> m abstract
bool = abstract @term @addr . A.Bool

asBool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m Bool
asBool v = send (AsBool @term @addr v pure)

string :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Text -> m abstract
string = abstract @term @addr . A.String

asString :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m Text
asString v = send (AsString @term @addr v pure)


lam :: Has (Domain term addr abstract) sig m => Named (Scope () term addr) -> m abstract
lam = abstract . A.Lam

-- FIXME: Support partial concretization of lambdas.
asLam :: Has (Domain term addr abstract) sig m => abstract -> m (Named (Scope () term addr))
asLam v = send (AsLam v pure)


record :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => [(Name, term addr)] -> m abstract
record = abstract @term . A.Record

-- FIXME: Support partial concretization of lambdas and records.
asRecord :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m [(Name, term addr)]
asRecord v = send (AsRecord v pure)


data Domain term addr abstract m k
  = Abstract (Intro term addr) (abstract                   -> m k)
  | AsBool   abstract          (Bool                       -> m k)
  | AsString abstract          (Text                       -> m k)
  | AsLam    abstract          (Named (Scope () term addr) -> m k)
  | AsRecord abstract          ([(Name, term addr)]        -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term addr abstract)
instance Effect   (Domain term addr abstract)
