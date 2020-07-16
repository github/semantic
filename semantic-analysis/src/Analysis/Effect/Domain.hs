{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Kind (Type)
import Data.Text (Text)
import Syntax.Scope (Scope)

unit :: Has (UnitDomain value) sig m => m value
unit = send Unit

data UnitDomain value (m :: Type -> Type) k where
  Unit :: UnitDomain value m value


bool :: Has (BoolDomain value) sig m => Bool -> m value
bool b = send (Bool b)

asBool :: Has (BoolDomain value) sig m => value -> m Bool
asBool v = send (AsBool v)

data BoolDomain value (m :: Type -> Type) k where
  Bool   :: Bool  -> BoolDomain value m value
  AsBool :: value -> BoolDomain value m Bool


string :: Has (StringDomain value) sig m => Text -> m value
string s = send (String s)

asString :: Has (StringDomain value) sig m => value -> m Text
asString v = send (AsString v)

data StringDomain value (m :: Type -> Type) k where
  String   :: Text  -> StringDomain value m value
  AsString :: value -> StringDomain value m Text


lam :: Has (FunctionDomain term addr value) sig m => Named (Scope () term addr) -> m value
lam b = send (Lam b)

-- FIXME: Support partial concretization of lambdas.
asLam :: Has (FunctionDomain term addr value) sig m => value -> m (Named (Scope () term addr))
asLam v = send (AsLam v)

data FunctionDomain term addr value (m :: Type -> Type) k where
  Lam   :: Named (Scope () term addr) -> FunctionDomain term addr value m value
  AsLam :: value                      -> FunctionDomain term addr value m (Named (Scope () term addr))


record :: Has (RecordDomain term addr value) sig m => [(Name, term addr)] -> m value
record fs = send (Record fs)

-- FIXME: Support partial concretization of records.
asRecord :: Has (RecordDomain term addr value) sig m => value -> m [(Name, term addr)]
asRecord v = send (AsRecord v)

data RecordDomain term addr value (m :: Type -> Type) k where
  Record   :: [(Name, term addr)] -> RecordDomain term addr value m value
  AsRecord :: value               -> RecordDomain term addr value m [(Name, term addr)]


type Domain term addr value
  =   UnitDomain value
  :+: BoolDomain value
  :+: StringDomain value
  :+: FunctionDomain term addr value
  :+: RecordDomain term addr value
