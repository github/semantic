{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Value where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Type as Type
import Data.Abstract.Value as Value
import Data.Scientific (Scientific)
import Prelude hiding (fail)
import Prologue

-- | A 'Monad' abstracting the evaluation of (and under) binding constructs (functions, methods, etc).
--
--   This allows us to abstract the choice of whether to evaluate under binders for different value types.
class (MonadAnalysis term value m, Show value) => MonadValue term value m where
  -- | Construct an abstract unit value.
  --   TODO: This might be the same as the empty tuple for some value types
  unit :: m value

  -- | Construct an abstract integral value.
  integer :: Prelude.Integer -> m value

  -- | Construct an abstract boolean value.
  boolean :: Bool -> m value

  -- | Construct an abstract string value.
  string :: ByteString -> m value

  -- | Construct a floating-point value.
  float :: Scientific -> m value

  -- | Construct an N-ary tuple of multiple (possibly-disjoint) values
  multiple :: [value] -> m value

  -- | Eliminate boolean values. TODO: s/boolean/truthy
  ifthenelse :: value -> m value -> m value -> m value

  -- | Evaluate an abstraction (a binder like a lambda or method definition).
  abstract :: [Name] -> Subterm term (m value) -> m value
  -- | Evaluate an application (like a function call).
  apply :: value -> [Subterm term (m value)] -> m value


-- | Construct a 'Value' wrapping the value arguments (if any).
instance ( MonadAddressable location (Value location term) m
         , MonadAnalysis term (Value location term) m
         , Show location
         , Show term
         )
         => MonadValue term (Value location term) m where

  unit    = pure . injValue $ Value.Unit
  integer = pure . injValue . Integer
  boolean = pure . injValue . Boolean
  string  = pure . injValue . Value.String
  float   = pure . injValue . Value.Float
  multiple vals =
    pure . injValue $ Value.Tuple vals

  ifthenelse cond if' else'
    | Just (Boolean b) <- prjValue cond = if b then if' else else'
    | otherwise = fail ("not defined for non-boolean conditions: " <> show cond)

  abstract names (Subterm body _) = injValue . Closure names body <$> askLocalEnv

  apply op params = do
    Closure names body env <- maybe (fail ("expected a closure, got: " <> show op)) pure (prjValue op)
    bindings <- foldr (\ (name, param) rest -> do
      v <- subtermValue param
      a <- alloc name
      assign a v
      envInsert name a <$> rest) (pure env) (zip names params)
    localEnv (mappend bindings) (evaluateTerm body)

-- | Discard the value arguments (if any), constructing a 'Type.Type' instead.
instance (Alternative m, MonadAnalysis term Type m, MonadFresh m) => MonadValue term Type m where
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
  multiple  = pure . Type.Product

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  apply op params = do
    tvar <- fresh
    paramTypes <- traverse subtermValue params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret
