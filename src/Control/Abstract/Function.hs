{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Control.Abstract.Function where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Monad.Effect.Fresh
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Data.Abstract.Type
import Prologue
import Prelude hiding (fail)

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class MonadEvaluator t v m => MonadFunction t v m where
  abstract :: [Name] -> Subterm t (m v) -> m v
  apply :: v -> [Subterm t (m v)] -> m v

instance ( FreeVariables t
         , MonadAddressable location (Value location t) m
         , MonadAnalysis t (Value location t) m
         , MonadEvaluator t (Value location t) m
         , Recursive t
         , Semigroup (Cell location (Value location t))
         )
         => MonadFunction t (Value location t) m where
  abstract names (Subterm body _) = inj . Closure names body <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail "expected a closure") pure (prj op)
    bindings <- foldr (\ (name, param) rest -> do
      v <- subtermValue param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (evaluateTerm body)

instance (Alternative m, MonadEvaluator t Type m, MonadFresh m) => MonadFunction t Type m where
  abstract names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (envInsert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Product tvars :-> ret)

  apply op params = do
    tvar <- fresh
    paramTypes <- traverse subtermValue params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret
