{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Analysis.Effect.Domain
( -- * Domain effect
  unit
, UnitDomain(..)
, bool
, asBool
, BoolDomain(..)
, string
, asString
, StringDomain(..)
, lam
, asLam
, FunctionDomain(..)
, record
, asRecord
, RecordDomain(..)
, Domain
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Name
import Control.Algebra
import Data.Text (Text)
import GHC.Generics (Generic1)
import Syntax.Scope (Scope)

unit :: Has (UnitDomain abstract) sig m => m abstract
unit = send (Unit pure)

data UnitDomain abstract m k
  = Unit (abstract -> m k)
  deriving (Functor, Generic1)

instance HFunctor (UnitDomain abstract)
instance Effect   (UnitDomain abstract)


bool :: Has (BoolDomain abstract) sig m => Bool -> m abstract
bool b = send (Bool b pure)

asBool :: Has (BoolDomain abstract) sig m => abstract -> m Bool
asBool v = send (AsBool v pure)

data BoolDomain abstract m k
  = Bool   Bool     (abstract -> m k)
  | AsBool abstract (Bool     -> m k)
  deriving (Functor, Generic1)

instance HFunctor (BoolDomain abstract)
instance Effect   (BoolDomain abstract)


string :: Has (StringDomain abstract) sig m => Text -> m abstract
string s = send (String s pure)

asString :: Has (StringDomain abstract) sig m => abstract -> m Text
asString v = send (AsString v pure)

data StringDomain abstract m k
  = String   Text     (abstract -> m k)
  | AsString abstract (Text     -> m k)
  deriving (Functor, Generic1)

instance HFunctor (StringDomain abstract)
instance Effect   (StringDomain abstract)


lam :: Has (FunctionDomain term addr abstract) sig m => Named (Scope () term addr) -> m abstract
lam b = send (Lam b pure)

-- FIXME: Support partial concretization of lambdas.
asLam :: Has (FunctionDomain term addr abstract) sig m => abstract -> m (Named (Scope () term addr))
asLam v = send (AsLam v pure)

data FunctionDomain term addr abstract m k
  = Lam   (Named (Scope () term addr)) (abstract -> m k)
  | AsLam abstract                     (Named (Scope () term addr)     -> m k)
  deriving (Functor, Generic1)

instance HFunctor (FunctionDomain term addr abstract)
instance Effect   (FunctionDomain term addr abstract)


record :: Has (RecordDomain term addr abstract) sig m => [(Name, term addr)] -> m abstract
record fs = send (Record fs pure)

-- FIXME: Support partial concretization of records.
asRecord :: Has (RecordDomain term addr abstract) sig m => abstract -> m [(Name, term addr)]
asRecord v = send (AsRecord v pure)

data RecordDomain term addr abstract m k
  = Record   [(Name, term addr)] (abstract -> m k)
  | AsRecord abstract            ([(Name, term addr)]     -> m k)
  deriving (Functor, Generic1)

instance HFunctor (RecordDomain term addr abstract)
instance Effect   (RecordDomain term addr abstract)


type Domain term addr abstract
  =   UnitDomain abstract
  :+: BoolDomain abstract
  :+: StringDomain abstract
  :+: FunctionDomain term addr abstract
  :+: RecordDomain term addr abstract
