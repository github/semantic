{-# LANGUAGE DataKinds, FunctionalDependencies, MultiParamTypeClasses, Rank2Types, GADTs, TypeOperators, DefaultSignatures, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Monad.Effect.Evaluatable
( Evaluatable(..)
, module Evaluator
, Recursive(..)
, Base
, Subterm(..)
, AbstractFunction(..)
, Analysis(..)
) where

import Analysis.Abstract.Evaluator as Evaluator
import Control.Monad.Effect.Addressable
import Control.Monad.Effect.Dead
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Internal
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Type as Type
import Data.Abstract.Value
import Data.Algebra
import Data.Functor.Classes
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Proxy
import Data.Semigroup
import Data.Term
import Data.Union (Apply)
import Prelude hiding (fail)
import qualified Data.Union as U


-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( AbstractFunction m term value
          , MonadAddressable value (LocationFor value) m
          , FreeVariables term
          , Ord (LocationFor value)
          , Semigroup (Cell (LocationFor value) value)
          , MonadEvaluator term value m
          )
          => SubtermAlgebra constr term (m value)
  default eval :: (FreeVariables term, Show1 constr, MonadFail m) => SubtermAlgebra constr term (m value)
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
    env <- getGlobalEnv                 -- Get the global environment after evaluation
                               -- since it might have been modified by the
                               -- evaluation above ^.

    -- Finally, evaluate the rest of the terms, but do so by calculating a new
    -- environment each time where the free variables in those terms are bound
    -- to the global environment.
    localEnv (const (bindEnv (liftFreeVariables (freeVariables . subterm) xs) env)) (eval xs)

class AbstractValue v => AbstractFunction m t v | v -> t where
  abstract :: [Name] -> Subterm t (m v) -> m v
  apply :: v -> [Subterm t (m v)] -> m v

instance ( Analysis t (Value location t) m
         , FreeVariables t
         , MonadAddressable (Value location t) location m
         , MonadEvaluator t (Value location t) m
         , Semigroup (Cell location (Value location t))
         )
         => AbstractFunction m t (Value location t) where
  -- FIXME: Can we store the action evaluating the body in the Value instead of the body term itself
  abstract names (Subterm body _) = inj . Closure names body <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail "expected a closure") pure (prj op :: Maybe (Closure location t))
    bindings <- foldr (\ (name, param) rest -> do
      v <- subtermValue param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (runAnalysis body)

instance Members '[Fresh, NonDetEff] effects => AbstractFunction (Evaluator effects t (Type t)) t (Type t) where
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


class Analysis term value m | m -> term, m -> value where
  runAnalysis :: term -> m value

instance ( AbstractFunction (Evaluator effects term value) term value
         , MonadAddressable value (LocationFor value) (Evaluator effects term value)
         , Evaluatable (Base term)
         , FreeVariables term
         , Ord (LocationFor value)
         , Recursive term
         , Semigroup (Cell (LocationFor value) value)
         )
         => Analysis term value (Evaluator effects term value) where
  runAnalysis = foldSubterms eval

newtype DeadAnalysis effects term value a = DeadAnalysis { runDeadAnalysis :: Evaluator effects term value a }

instance ( AbstractFunction (Evaluator effects term value) term value
         , Corecursive term
         , Evaluatable (Base term)
         , FreeVariables term
         , Member (State (Dead term)) effects
         , MonadAddressable value (LocationFor value) (Evaluator effects term value)
         , Ord (LocationFor value)
         , Ord term
         , Recursive term
         , Semigroup (Cell (LocationFor value) value)
         )
         => Analysis term value (DeadAnalysis effects term value) where
  runAnalysis = DeadAnalysis . foldSubterms (\ term -> do
    revive (embed (fmap subterm term))
    eval term)

instance Member (State (Dead term)) effects => MonadDead term (Evaluator effects term value) where
  killAll t = Evaluator (killAll t)
  revive t = Evaluator (revive t)
