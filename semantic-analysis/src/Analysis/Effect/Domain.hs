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

import Analysis.Functor.Named
import Control.Algebra
import Data.Text (Text)
import GHC.Generics (Generic1)
import Syntax.Scope (Scope)

unit :: Has (UnitDomain value) sig m => m value
unit = send (Unit pure)

data UnitDomain value m k
  = Unit (value -> m k)
  deriving (Functor, Generic1)

instance HFunctor (UnitDomain value)
instance Effect   (UnitDomain value)


bool :: Has (BoolDomain value) sig m => Bool -> m value
bool b = send (Bool b pure)

asBool :: Has (BoolDomain value) sig m => value -> m Bool
asBool v = send (AsBool v pure)

data BoolDomain value m k
  = Bool   Bool  (value -> m k)
  | AsBool value (Bool  -> m k)
  deriving (Functor, Generic1)

instance HFunctor (BoolDomain value)
instance Effect   (BoolDomain value)


string :: Has (StringDomain value) sig m => Text -> m value
string s = send (String s pure)

asString :: Has (StringDomain value) sig m => value -> m Text
asString v = send (AsString v pure)

data StringDomain value m k
  = String   Text  (value -> m k)
  | AsString value (Text  -> m k)
  deriving (Functor, Generic1)

instance HFunctor (StringDomain value)
instance Effect   (StringDomain value)


lam :: Has (FunctionDomain term addr value) sig m => Named (Scope () term addr) -> m value
lam b = send (Lam b pure)

-- FIXME: Support partial concretization of lambdas.
asLam :: Has (FunctionDomain term addr value) sig m => value -> m (Named (Scope () term addr))
asLam v = send (AsLam v pure)

data FunctionDomain term addr value m k
  = Lam   (Named (Scope () term addr)) (value                      -> m k)
  | AsLam value                        (Named (Scope () term addr) -> m k)
  deriving (Functor, Generic1)

instance HFunctor (FunctionDomain term addr value)
instance Effect   (FunctionDomain term addr value)


record :: Has (RecordDomain term addr value) sig m => [(Name, term addr)] -> m value
record fs = send (Record fs pure)

-- FIXME: Support partial concretization of records.
asRecord :: Has (RecordDomain term addr value) sig m => value -> m [(Name, term addr)]
asRecord v = send (AsRecord v pure)

data RecordDomain term addr value m k
  = Record   [(Name, term addr)] (value               -> m k)
  | AsRecord value               ([(Name, term addr)] -> m k)
  deriving (Functor, Generic1)

instance HFunctor (RecordDomain term addr value)
instance Effect   (RecordDomain term addr value)


type Domain term addr value
  =   UnitDomain value
  :+: BoolDomain value
  :+: StringDomain value
  :+: FunctionDomain term addr value
  :+: RecordDomain term addr value
