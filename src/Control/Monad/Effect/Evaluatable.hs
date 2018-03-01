{-# LANGUAGE DataKinds, FunctionalDependencies, MultiParamTypeClasses, Rank2Types, GADTs, TypeOperators, DefaultSignatures, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Monad.Effect.Evaluatable
( Evaluatable(..)
, module Addressable
, module Evaluator
, Recursive(..)
, Base
, Subterm(..)
, MonadFunctionAbstraction(..)
) where

import Control.Abstract.Addressable as Addressable
import Control.Abstract.Evaluator as Evaluator
import Control.Applicative (Alternative(..))
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Type as Type
import Data.Abstract.Value
import Data.Algebra
import Data.Functor.Classes
import Data.Functor.Foldable (Base, Recursive(..), project)
import Data.Proxy
import Data.Semigroup
import Data.Term
import Data.Union (Apply)
import Prelude hiding (fail)
import qualified Data.Union as U


-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( AbstractValue value
          , FreeVariables term
          , MonadAddressable (LocationFor value) value m
          , MonadEvaluator term value m
          , MonadFunctionAbstraction term value m
          , Ord (LocationFor value)
          , Semigroup (Cell (LocationFor value) value)
          )
          => SubtermAlgebra constr term (m value)
  default eval :: (FreeVariables term, MonadFail m, Show1 constr) => SubtermAlgebra constr term (m value)
  eval expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = U.apply (Proxy :: Proxy Evaluatable) eval

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
  eval []     = pure unit      -- Return unit value if this is an empty list of terms
  eval [x]    = subtermValue x -- Return the value for the last term
  eval (x:xs) = do
    _ <- subtermValue x        -- Evaluate the head term
    env <- getGlobalEnv        -- Get the global environment after evaluation
                               -- since it might have been modified by the
                               -- evaluation above ^.

    -- Finally, evaluate the rest of the terms, but do so by calculating a new
    -- environment each time where the free variables in those terms are bound
    -- to the global environment.
    localEnv (const (bindEnv (liftFreeVariables (freeVariables . subterm) xs) env)) (eval xs)

class MonadEvaluator t v m => MonadFunctionAbstraction t v m where
  abstract :: [Name] -> Subterm t (m v) -> m v
  apply :: v -> [Subterm t (m v)] -> m v

instance ( Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable location (Value location t) m
         , MonadEvaluator t (Value location t) m
         , Recursive t
         , Semigroup (Cell location (Value location t))
         )
         => MonadFunctionAbstraction t (Value location t) m where
  -- FIXME: Can we store the action evaluating the body in the Value instead of the body term itself
  abstract names (Subterm body _) = inj . Closure names body <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail "expected a closure") pure (prj op :: Maybe (Closure location t))
    bindings <- foldr (\ (name, param) rest -> do
      v <- subtermValue param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (foldSubterms eval body)

instance (Alternative m, MonadEvaluator t (Type.Type t) m, MonadFresh m) => MonadFunctionAbstraction t (Type t) m where
  abstract names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (envInsert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Type.Product tvars :-> ret)

  apply op params = do
    tvar <- fresh
    paramTypes <- traverse subtermValue params
    _ :-> ret <- op `unify` (Type.Product paramTypes :-> Var tvar)
    pure ret
