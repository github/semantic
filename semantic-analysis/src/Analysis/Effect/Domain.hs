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
  unit
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

import           Analysis.Name
import           Control.Algebra
import           Data.Text (Text)
import           GHC.Generics (Generic1)
import           Syntax.Scope (Scope)

unit :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => m abstract
unit = send (Unit @term @addr pure)

bool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Bool -> m abstract
bool b = send (Bool @term @addr b pure)

asBool :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m Bool
asBool v = send (AsBool @term @addr v pure)

string :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => Text -> m abstract
string s = send (String @term @addr s pure)

asString :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m Text
asString v = send (AsString @term @addr v pure)


lam :: Has (Domain term addr abstract) sig m => Named (Scope () term addr) -> m abstract
lam b = send (Lam b pure)

-- FIXME: Support partial concretization of lambdas.
asLam :: Has (Domain term addr abstract) sig m => abstract -> m (Named (Scope () term addr))
asLam v = send (AsLam v pure)


record :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => [(Name, term addr)] -> m abstract
record fs = send (Record fs pure)

-- FIXME: Support partial concretization of records.
asRecord :: forall term addr abstract m sig . Has (Domain term addr abstract) sig m => abstract -> m [(Name, term addr)]
asRecord v = send (AsRecord v pure)


data Domain term addr abstract m k
  = Unit                                  (abstract                   -> m k)
  | Bool     Bool                         (abstract                   -> m k)
  | AsBool   abstract                     (Bool                       -> m k)
  | String   Text                         (abstract                   -> m k)
  | AsString abstract                     (Text                       -> m k)
  | Lam      (Named (Scope () term addr)) (abstract                   -> m k)
  | AsLam    abstract                     (Named (Scope () term addr) -> m k)
  | Record   [(Name, term addr)]          (abstract                   -> m k)
  | AsRecord abstract                     ([(Name, term addr)]        -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term addr abstract)
instance Effect   (Domain term addr abstract)
