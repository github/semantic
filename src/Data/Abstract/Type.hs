{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the MonadValue instance, which requires MonadEvaluator to resolve its functional dependency.
module Data.Abstract.Type
  ( Type (..)
  , TypeError (..)
  , unify
  ) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Environment as Env
import Data.Align (alignWith)
import Data.Semigroup.Reducer (Reducer)
import Prelude
import Prologue hiding (TypeError)

type TName = Int

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
  | Hole                -- ^ The hole type.
  deriving (Eq, Ord, Show)

-- TODO: À la carte representation of types.

data TypeError resume where
  NumOpError       :: Type -> Type -> TypeError Type
  BitOpError       :: Type -> Type -> TypeError Type
  UnificationError :: Type -> Type -> TypeError Type

deriving instance Show (TypeError resume)

instance Show1 TypeError where
  liftShowsPrec _ _ _ (NumOpError l r)       = showString "NumOpError " . shows [l, r]
  liftShowsPrec _ _ _ (BitOpError l r)       = showString "BitOpError " . shows [l, r]
  liftShowsPrec _ _ _ (UnificationError l r) = showString "UnificationError " . shows [l, r]

instance Eq1 TypeError where
  liftEq _ (BitOpError a b) (BitOpError c d)             = a == c && b == d
  liftEq _ (NumOpError a b) (NumOpError c d)             = a == c && b == d
  liftEq _ (UnificationError a b) (UnificationError c d) = a == c && b == d
  liftEq _ _ _                                           = False

-- | Unify two 'Type's.
unify :: (Effectful m, Applicative (m effects), Member (Resumable TypeError) effects) => Type -> Type -> m effects Type
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify a Null = pure a
unify Null b = pure b
-- FIXME: this should be constructing a substitution.
unify (Var _) b = pure b
unify a (Var _) = pure a
unify (Product as) (Product bs) = Product <$> sequenceA (alignWith (these pure pure unify) as bs)
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = throwResumable (UnificationError t1 t2)

instance Ord location => ValueRoots location Type where
  valueRoots _ = mempty


instance AbstractHole Type where
  hole = Hole

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Alternative (m effects)
         , Member Fresh effects
         , Member (Resumable TypeError) effects
         , MonadAddressable location effects m
         , MonadEvaluator location term Type effects m
         , Reducer Type (Cell location Type)
         )
      => MonadValue location Type effects m where
  lambda names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> raise fresh
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

  asString t = unify t String $> ""
  asPair t   = do
    t1 <- raise fresh
    t2 <- raise fresh
    unify t (Product [Var t1, Var t2]) $> (Var t1, Var t2)
  asBool t   = unify t Bool *> (pure True <|> pure False)

  isHole ty = pure (ty == Hole)

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  liftNumeric _ Float      = pure Float
  liftNumeric _ Int        = pure Int
  liftNumeric _ t          = throwResumable (NumOpError t Hole)

  liftNumeric2 _ left right = case (left, right) of
    (Float, Int) -> pure Float
    (Int, Float) -> pure Float
    _            -> unify left right

  liftBitwise _ Int = pure Int
  liftBitwise _ t   = throwResumable (BitOpError t Hole)

  liftBitwise2 _ Int Int = pure Int
  liftBitwise2 _ t1 t2   = throwResumable (BitOpError t1 t2)

  liftComparison (Concrete _) left right = case (left, right) of
    (Float, Int) ->                     pure Bool
    (Int, Float) ->                     pure Bool
    _                 -> unify left right $> Bool
  liftComparison Generalized left right = case (left, right) of
    (Float, Int) ->                     pure Int
    (Int, Float) ->                     pure Int
    _                 -> unify left right $> Bool

  call op params = do
    tvar <- raise fresh
    paramTypes <- sequenceA params
    let needed = Product paramTypes :-> Var tvar
    unified <- op `unify` needed
    case unified of
      _ :-> ret -> pure ret
      gotten    -> throwResumable (UnificationError needed gotten)

  loop f = f empty
