{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( Evaluatable(..)
, module X
, require
, load
) where

import Control.Abstract.Addressable as X
import Control.Abstract.Analysis as X
import Control.Abstract.Value as X
import qualified Data.Abstract.Environment as Env
import qualified Data.Abstract.Exports as Exports
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable
import Data.Abstract.Value
import Data.Functor.Classes
import Data.Proxy
import Data.Semigroup.Foldable
import Data.Semigroup.App
import Data.Term
import Prelude hiding (fail)
import Prologue


-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( FreeVariables term
          , MonadAddressable (LocationFor value) value m
          , MonadAnalysis term value m
          , MonadValue value m
          , Show (LocationFor value)
          , MonadThrow Prelude.String value m
          , MonadThrow (EvaluateModule term) value m
          )
       => SubtermAlgebra constr term (m value)
  default eval :: (MonadThrow Prelude.String value m, Show1 constr) => SubtermAlgebra constr term (m value)
  eval expr = throwException $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = Prologue.apply (Proxy :: Proxy Evaluatable) eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval = eval . termFOut


-- Instances

-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe unit (runApp . foldMap1 (App . subtermValue)) . nonEmpty


-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: ( MonadAnalysis term value m
           , MonadThrow (EvaluateModule term) value m
           , MonadValue value m
           )
        => ModuleName
        -> m (EnvironmentFor value, value)
require name = getModuleTable >>= maybe (load name) pure . moduleTableLookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: forall term value m
     .  ( MonadAnalysis term value m
        , MonadThrow (EvaluateModule term) value m
        , MonadValue value m
        )
     => ModuleName
     -> m (EnvironmentFor value, value)
load name = askModuleTable >>= maybe notFound evalAndCache . moduleTableLookup name
  where
    notFound = fail ("cannot load module: " <> show name)

    evalAndCache []     = (,) <$> pure mempty <*> unit
    evalAndCache [x]    = evalAndCache' x
    evalAndCache (x:xs) = do
      (env, _) <- evalAndCache' x
      (env', v') <- evalAndCache xs
      pure (env <> env', v')

    evalAndCache' x = do
      v <- throwException (EvaluateModule x) :: m value
      env <- filterEnv <$> getExports <*> getEnv
      modifyModuleTable (moduleTableInsert name (env, v))
      pure (env, v)

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports.Exports l a -> Env.Environment l a -> Env.Environment l a
    filterEnv ports env
      | Exports.null ports = env
      | otherwise = Exports.toEnvironment ports <> Env.overwrite (Exports.aliases ports) env
