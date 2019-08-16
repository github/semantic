{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeOperators #-}

module ScopeDump ( ScopeDump (..), runScopeDump ) where

import qualified Analysis.Eval as Eval
import           Control.Effect
import           Control.Effect.Fail
import           Control.Effect.Reader
import qualified Data.Aeson as Aeson
import           Data.Core
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Loc
import           Data.Name
import           Data.Term

data ScopeDump
  = DumpRecord (HashMap Name ScopeDump)
  | Nil
    deriving (Eq, Show)

instance Semigroup ScopeDump where
  DumpRecord lhs <> DumpRecord rhs = DumpRecord (HashMap.unionWith (<>) lhs rhs)
  Nil <> rhs = rhs
  lhs <> Nil = lhs

instance Aeson.ToJSON ScopeDump where
  toJSON (DumpRecord hs) = Aeson.Object (fmap Aeson.toJSON hs)
  toJSON Nil             = Aeson.Null

scopeDump :: Applicative m => Eval.Analysis term () ScopeDump m
scopeDump = Eval.Analysis {..} where
  alloc _name = pure ()
  bind _name _addr within = within
  lookupEnv _name = pure (Just ())
  deref _addr = pure (Just Nil)
  assign _addr _value = pure ()
  abstract _thing _name _term = pure Nil
  apply _thing _fn _arg = pure Nil
  unit = pure Nil
  bool _b = pure Nil
  asBool _val = pure True
  string _s = pure Nil
  asString _val = pure ""
  record = pure . DumpRecord . HashMap.fromList
  _addr ... _name = pure (Just ())

unhelpful :: Loc
unhelpful = Loc "<interactive>" emptySpan

runScopeDump :: Term (Ann :+: Core) Name -> Either String ScopeDump
runScopeDump
  = run
  . runFail
  . runReader unhelpful
  . fix (Eval.eval scopeDump)
