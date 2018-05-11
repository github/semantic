{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Type
  ( Type (..)
  , TypeError (..)
  , runTypeError
  , unify
  ) where

import Control.Abstract
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
  | Array Type          -- ^ Arrays.
  | Hash [(Type, Type)] -- ^ Heterogenous key-value maps.
  | Object              -- ^ Objects. Once we have some notion of inheritance we'll need to store a superclass.
  | Null                -- ^ The null type. Unlike 'Unit', this unifies with any other type.
  | Hole                -- ^ The hole type.
  deriving (Eq, Ord, Show)

-- TODO: À la carte representation of types.

data TypeError resume where
  NumOpError       :: Type -> Type -> TypeError Type
  UnificationError :: Type -> Type -> TypeError Type
  SubscriptError   :: Type -> Type -> TypeError Type

deriving instance Show (TypeError resume)

instance Show1 TypeError where
  liftShowsPrec _ _ _ (NumOpError l r)       = showString "NumOpError " . shows [l, r]
  liftShowsPrec _ _ _ (UnificationError l r) = showString "UnificationError " . shows [l, r]
  liftShowsPrec _ _ _ (SubscriptError l r)   = showString "SubscriptError " . shows [l, r]

instance Eq1 TypeError where
  liftEq eq (NumOpError a b) (NumOpError c d)             = a `eq` c && b `eq` d
  liftEq eq (UnificationError a b) (UnificationError c d) = a `eq` c && b `eq` d
  liftEq _ _ _                                           = False

runTypeError :: Evaluator location value (Resumable TypeError ': effects) a -> Evaluator location value effects (Either (SomeExc TypeError) a)
runTypeError = runResumable

-- | Unify two 'Type's.
unify :: (Effectful m, Applicative (m effects), Member (Resumable TypeError) effects) => Type -> Type -> m effects Type
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify a Null = pure a
unify Null b = pure b
-- FIXME: this should be constructing a substitution.
unify (Var _) b = pure b
unify a (Var _) = pure a
unify (Array t1) (Array t2) = Array <$> unify t1 t2
unify (Product as) (Product bs) = Product <$> sequenceA (alignWith (these pure pure unify) as bs)
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = throwResumable (UnificationError t1 t2)

instance Ord location => ValueRoots location Type where
  valueRoots _ = mempty


instance AbstractHole Type where
  hole = Hole

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Addressable location effects
         , Members '[ Fresh
                    , NonDet
                    , Resumable TypeError
                    , State (Environment location Type)
                    , State (Heap location (Cell location) Type)
                    ] effects
         , Reducer Type (Cell location Type)
         )
      => AbstractValue location Type effects where
  closure names _ body = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      (env, tvars) <- rest
      pure (Env.insert name a env, tvar : tvars)) (pure (emptyEnv, [])) names
    ret <- localEnv (mergeEnvs env) body
    pure (Product tvars :-> ret)

  unit       = pure Unit
  integer _  = pure Int
  boolean _  = pure Bool
  string _   = pure String
  float _    = pure Float
  symbol _   = pure Symbol
  rational _ = pure Rational
  multiple   = pure . Product
  array fields = do
    var <- fresh
    Array <$> foldr (\ t1 -> (unify t1 =<<)) (pure (Var var)) fields
  hash       = pure . Hash
  kvPair k v = pure (Product [k, v])

  null          = pure Null

  klass _ _ _   = pure Object
  namespace _ _ = pure Unit

  scopedEnvironment _ = pure (Just emptyEnv)

  asString t = unify t String $> ""
  asPair t   = do
    t1 <- fresh
    t2 <- fresh
    unify t (Product [Var t1, Var t2]) $> (Var t1, Var t2)
  asBool t   = unify t Bool *> (pure True <|> pure False)

  isHole ty = pure (ty == Hole)

  index (Array mem) Int   = pure mem
  index (Product (mem:_)) Int = pure mem
  index a b                   = throwResumable (SubscriptError a b)

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  liftNumeric _ Float      = pure Float
  liftNumeric _ Int        = pure Int
  liftNumeric _ t          = throwResumable (NumOpError t Hole)

  liftNumeric2 _ left right = case (left, right) of
    (Float, Int) -> pure Float
    (Int, Float) -> pure Float
    _            -> unify left right

  liftBitwise _ = unify Int
  liftBitwise2 _ t1 t2   = unify Int t1 >>= flip unify t2

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
    let needed = Product paramTypes :-> Var tvar
    unified <- op `unify` needed
    case unified of
      _ :-> ret -> pure ret
      gotten    -> throwResumable (UnificationError needed gotten)

  loop f = f empty
