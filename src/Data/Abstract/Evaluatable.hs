{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( Evaluatable(..)
, module Addressable
, module Analysis
, module FreeVariables
, module Value
, MonadEvaluator(..)
, Imperative(..)
) where

import Control.Abstract.Addressable as Addressable
import Control.Abstract.Analysis as Analysis
import Control.Abstract.Value as Value
import Data.Abstract.FreeVariables as FreeVariables
import Data.Abstract.Value
import Data.Functor.Classes
import Data.Proxy
import Data.Semigroup.Foldable
import Data.Term
import Prelude hiding (fail)
import Prologue


-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( FreeVariables term
          , MonadAddressable (LocationFor value) value m
          , MonadAnalysis term value m
          , MonadValue term value m
          )
       => SubtermAlgebra constr term (m value)
  default eval :: (MonadFail m, Show1 constr) => SubtermAlgebra constr term (m value)
  eval expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = Prologue.apply (Proxy :: Proxy Evaluatable) eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval In{..} = eval termFOut


-- Instances

-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  eval = maybe unit (runImperative . foldMap1 (Imperative . subtermValue)) . nonEmpty


-- | A 'Semigroup' providing an imperative context which extends the local environment with new bindings.
newtype Imperative m a = Imperative { runImperative :: m a }

instance MonadEnvironment value m => Semigroup (Imperative m a) where
  Imperative a <> Imperative b = Imperative $ a *> do
    env <- getGlobalEnv
    localEnv (<> env) b

instance MonadValue term value m => Monoid (Imperative m value) where
  mempty = Imperative unit
  mappend = (<>)
