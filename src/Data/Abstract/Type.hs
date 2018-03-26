{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Type where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Environment as Env
import Data.Align (alignWith)
import Prelude hiding (fail)
import Prologue

-- | A datatype representing primitive types and combinations thereof.
data Type
  = Int                 -- ^ Primitive int type.
  | Bool                -- ^ Primitive boolean type.
  | String              -- ^ Primitive string type.
  | Symbol              -- ^ Type of unique symbols.
  | Unit                -- ^ The unit type.
  | Float               -- ^ Floating-point type.
  | Rational            -- ^ Rational type.
  | Type :-> Type       -- ^ Binary function types.
  | Var TName           -- ^ A type variable.
  | Product [Type]      -- ^ N-ary products.
  | Array [Type]        -- ^ Arrays. Note that this is heterogenous.
  | Hash [(Type, Type)] -- ^ Heterogenous key-value maps.
  | Object              -- ^ Objects. Once we have some notion of inheritance we'll need to store a superclass.
  | Null                -- ^ The null type. Unlike 'Unit', this unifies with any other type.
  deriving (Eq, Ord, Show)

-- TODO: À la carte representation of types.


-- | Unify two 'Type's.
unify :: MonadFail m => Type -> Type -> m Type
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify a Null = pure a
unify Null b = pure b
-- FIXME: this should be constructing a substitution.
unify (Var _) b = pure b
unify a (Var _) = pure a
unify (Product as) (Product bs) = Product <$> sequenceA (alignWith (these pure pure unify) as bs)
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = fail ("cannot unify " ++ show t1 ++ " with " ++ show t2)


type instance LocationFor Type = Monovariant

instance ValueRoots Type where
  valueRoots _ = mempty


-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance (Alternative m, MonadEnvironment Type m, MonadFail m, MonadFresh m, MonadHeap Type m) => MonadValue Type m where
  abstract names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (Env.insert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Product tvars :-> ret)

  unit       = pure Unit
  integer _  = pure Int
  boolean _  = pure Bool
  string _   = pure String
  float _    = pure Float
  symbol _   = pure Symbol
  rational _ = pure Rational
  multiple   = pure . Product
  array      = pure . Array
  hash       = pure . Hash
  kvPair k v = pure (Product [k, v])

  null          = pure Null

  klass _ _ _   = pure Object
  namespace _ _ = pure Unit

  scopedEnvironment _ = pure mempty

  asString _ = fail "Must evaluate to Value to use asString"
  asPair _   = fail "Must evaluate to Value to use asPair"

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  liftNumeric _ Float = pure Float
  liftNumeric _ Int        = pure Int
  liftNumeric _ _          = fail "Invalid type in unary numeric operation"

  liftNumeric2 _ left right = case (left, right) of
    (Float, Int) -> pure Float
    (Int, Float) -> pure Float
    _                 -> unify left right

  liftBitwise _ Int = pure Int
  liftBitwise _ t   = fail ("Invalid type passed to unary bitwise operation: " <> show t)

  liftBitwise2 _ Int Int = pure Int
  liftBitwise2 _ t1 t2   = fail ("Invalid types passed to binary bitwise operation: " <> show (t1, t2))

  liftComparison (Concrete _) left right = case (left, right) of
    (Float, Int) ->                     pure Bool
    (Int, Float) ->                     pure Bool
    _                 -> unify left right $> Bool
  liftComparison Generalized left right = case (left, right) of
    (Float, Int) ->                     pure Int
    (Int, Float) ->                     pure Int
    _                 -> unify left right $> Bool

  apply op params = do
    tvar <- fresh
    paramTypes <- sequenceA params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret

  loop f = f empty
