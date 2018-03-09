{-# LANGUAGE DataKinds, FunctionalDependencies, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Value where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Monad.Effect.Fresh
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value as Value
import Data.Abstract.Type as Type
import Data.Scientific (Scientific)
import Prologue
import Prelude hiding (fail)

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class Monad m => MonadValue term value m | m -> term, m -> value where
  -- | Construct an abstract unit value.
  unit :: m value

  -- | Construct an abstract integral value.
  integer :: Prelude.Integer -> m value

  -- | Construct an abstract boolean value.
  boolean :: Bool -> m value

  -- | Construct an abstract string value.
  string :: ByteString -> m value

  -- | Construct a floating-point value.
  float :: Scientific -> m value

  -- | Construct an abstract interface value.
  interface :: value -> m value

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> m value -> m value -> m value

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  abstract :: [Name] -> Subterm term (m value) -> m value
  -- | Evaluate an application (like a function call).
  apply :: value -> [Subterm term (m value)] -> m value

  -- | Extract the environment from an interface value.
  environment :: value -> m (EnvironmentFor value)

-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( FreeVariables term
         , MonadAddressable location (Value location term) (m term (Value location term) effects)
         , MonadAnalysis term (Value location term) effects m
         , MonadEvaluator term (Value location term) effects m
         , Recursive term
         , Semigroup (Cell location (Value location term))
         )
         => MonadValue term (Value location term) (m term (Value location term) effects) where

  unit    = pure $ inj Value.Unit
  integer = pure . inj . Integer
  boolean = pure . inj . Boolean
  string  = pure . inj . Value.String
  float   = pure . inj . Value.Float
  interface v = inj . Value.Interface v <$> getGlobalEnv

  ifthenelse cond if' else'
    | Just (Boolean b) <- prj cond = if b then if' else else'
    | otherwise = fail "not defined for non-boolean conditions"

  abstract names (Subterm body _) = inj . Closure names body <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail "expected a closure") pure (prj op)
    bindings <- foldr (\ (name, param) rest -> do
      v <- subtermValue param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (evaluateTerm body)

  environment v
    | Just (Interface _ env) <- prj v = pure env
    | otherwise                       = pure mempty

-- | Discard the value arguments (if any), constructing a 'Type.Type' instead.
instance (Alternative (m term Type effects), MonadEvaluator term Type effects m, MonadFresh (m term Type effects)) => MonadValue  term Type (m term Type effects) where
  abstract names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (envInsert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Product tvars :-> ret)

  unit      = pure Type.Unit
  integer _ = pure Int
  boolean _ = pure Bool
  string _  = pure Type.String
  float _   = pure Type.Float
  -- TODO
  interface = undefined

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  apply op params = do
    tvar <- fresh
    paramTypes <- traverse subtermValue params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret

  -- TODO
  environment = undefined
