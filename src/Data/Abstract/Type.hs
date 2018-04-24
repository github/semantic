{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances #-}
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
import Prelude hiding (fail)
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
  NoValueError     :: Type -> a -> TypeError a
  NumOpError       :: Type -> Type -> TypeError Type
  BitOpError       :: Type -> Type -> TypeError Type
  UnificationError :: Type -> Type -> TypeError Type

deriving instance Show resume => Show (TypeError resume)

instance Show1 TypeError where
  liftShowsPrec _ _ _ (NoValueError v _)     = showString "NoValueError " . shows v
  liftShowsPrec _ _ _ (NumOpError l r)       = showString "NumOpError " . shows [l, r]
  liftShowsPrec _ _ _ (BitOpError l r)       = showString "BitOpError " . shows [l, r]
  liftShowsPrec _ _ _ (UnificationError l r) = showString "UnificationError " . shows [l, r]

instance Eq1 TypeError where
  liftEq _ (NoValueError a _) (NoValueError b _)         = a == b
  liftEq _ (BitOpError a b) (BitOpError c d)             = a == c && b == d
  liftEq _ (NumOpError a b) (NumOpError c d)             = a == c && b == d
  liftEq _ (UnificationError a b) (UnificationError c d) = a == c && b == d
  liftEq _ _ _                                           = False

-- | Unify two 'Type's.
unify :: MonadResume TypeError m => Type -> Type -> m Type
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


-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Alternative m
         , MonadAddressable location m
         , MonadEnvironment location Type m
         , MonadFail m
         , MonadFresh m
         , MonadHeap location Type m
         , MonadResume TypeError m
         , Reducer Type (Cell location Type)
         )
      => MonadValue location Type m where
  lambda names (Subterm _ body) = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (Env.insert name a env, tvar : tvars)) (pure mempty) names
    ret <- localEnv (mappend env) body
    pure (Product tvars :-> ret)

  hole       = pure Hole
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

  asString _ = throwResumable (NoValueError String "")
  asPair _   = throwResumable (NoValueError (Product []) (Hole, Hole))
  asBool _   = throwResumable (NoValueError Bool True)

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
    tvar <- fresh
    paramTypes <- sequenceA params
    _ :-> ret <- op `unify` (Product paramTypes :-> Var tvar)
    pure ret

  loop f = f empty
