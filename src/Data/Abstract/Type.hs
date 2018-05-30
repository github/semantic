{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Type
  ( Type (..)
  , TypeError (..)
  , runTypeError
  , runTypeErrorWith
  , unify
  ) where

import Control.Abstract
import Data.Abstract.Environment as Env
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.Reducer (Reducer)
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
  | Type :* Type        -- ^ Binary products.
  | Type :+ Type        -- ^ Binary sums.
  | Void                -- ^ Uninhabited void type.
  | Array Type          -- ^ Arrays.
  | Hash [(Type, Type)] -- ^ Heterogenous key-value maps.
  | Object              -- ^ Objects. Once we have some notion of inheritance we'll need to store a superclass.
  | Null                -- ^ The null type. Unlike 'Unit', this unifies with any other type.
  | Hole                -- ^ The hole type.
  deriving (Eq, Ord, Show)

infixl 6 :+
infixl 7 :*
infixr 0 :->

newtype Product = Product { getProduct :: Type }

instance Semigroup Product where
  Product a <> Product b = Product (a :* b)

instance Monoid Product where
  mempty = Product Unit
  mappend = (<>)

oneOrMoreProduct :: NonEmpty Type -> Type
oneOrMoreProduct = getProduct . foldMap1 Product

zeroOrMoreProduct :: [Type] -> Type
zeroOrMoreProduct = maybe Unit oneOrMoreProduct . nonEmpty

-- TODO: À la carte representation of types.

-- | Errors representing failures in typechecking. Note that we should in general constrain allowable types by 'unify'ing, and thus throwing 'UnificationError's when constraints aren’t met, in order to allow uniform resumption with one or the other parameter type.
data TypeError resume where
  UnificationError :: Type -> Type -> TypeError Type

deriving instance Eq   (TypeError resume)
deriving instance Ord  (TypeError resume)
deriving instance Show (TypeError resume)

instance Eq1   TypeError where liftEq      _ (UnificationError a1 b1) (UnificationError a2 b2) = a1 == a2 && b1 == b2
instance Ord1  TypeError where liftCompare _ (UnificationError a1 b1) (UnificationError a2 b2) = compare a1 a2 <> compare b1 b2
instance Show1 TypeError where liftShowsPrec _ _ = showsPrec


runTypeError :: Effectful m => m (Resumable TypeError ': effects) a -> m effects (Either (SomeExc TypeError) a)
runTypeError = runResumable

runTypeErrorWith :: Effectful m => (forall resume . TypeError resume -> m effects resume) -> m (Resumable TypeError ': effects) a -> m effects a
runTypeErrorWith = runResumableWith


-- | Unify two 'Type's.
unify :: (Effectful m, Applicative (m effects), Member (Resumable TypeError) effects) => Type -> Type -> m effects Type
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify a Null = pure a
unify Null b = pure b
-- FIXME: this should be constructing a substitution.
unify (Var _) b = pure b
unify a (Var _) = pure a
unify (Array t1) (Array t2) = Array <$> unify t1 t2
-- FIXME: unifying with sums should distribute nondeterministically.
-- FIXME: ordering shouldn’t be significant for undiscriminated sums.
unify (a1 :+ b1) (a2 :+ b2) = (:+) <$> unify a1 a2 <*> unify b1 b2
unify (a1 :* b1) (a2 :* b2) = (:*) <$> unify a1 a2 <*> unify b1 b2
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = throwResumable (UnificationError t1 t2)

instance Ord location => ValueRoots location Type where
  valueRoots _ = mempty


instance AbstractHole Type where
  hole = Hole

instance AbstractIntro Type where
  unit       = Unit
  integer _  = Int
  boolean _  = Bool
  string _   = String
  float _    = Float
  symbol _   = Symbol
  rational _ = Rational
  multiple   = zeroOrMoreProduct
  hash       = Hash
  kvPair k v = k :* v

  null        = Null


instance ( Member (Allocator location Type) effects
         , Member Fresh effects
         , Member (Resumable TypeError) effects
         , Member (Return Type) effects
         , Member (State (Environment location)) effects
         , Member (State (Heap location (Cell location) Type)) effects
         , Ord location
         , Reducer Type (Cell location Type)
         )
      => AbstractFunction location Type effects where
  closure names _ body = do
    (env, tvars) <- foldr (\ name rest -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      bimap (Env.insert name a) (tvar :) <$> rest) (pure (emptyEnv, [])) names
    (zeroOrMoreProduct tvars :->) <$> locally (bindAll env *> body `catchReturn` \ (Return value) -> pure value)

  call op params = do
    tvar <- fresh
    paramTypes <- sequenceA params
    let needed = zeroOrMoreProduct paramTypes :-> Var tvar
    unified <- op `unify` needed
    case unified of
      _ :-> ret -> pure ret
      gotten    -> throwResumable (UnificationError needed gotten)


-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Member (Allocator location Type) effects
         , Member Fresh effects
         , Member NonDet effects
         , Member (Resumable TypeError) effects
         , Member (Return Type) effects
         , Member (State (Environment location)) effects
         , Member (State (Heap location (Cell location) Type)) effects
         , Ord location
         , Reducer Type (Cell location Type)
         )
      => AbstractValue location Type effects where
  array fields = do
    var <- fresh
    Array <$> foldr (\ t1 -> (unify t1 =<<)) (pure (Var var)) fields

  klass _ _ _   = pure Object
  namespace _ _ = pure Unit

  scopedEnvironment _ = pure (Just emptyEnv)

  asString t = unify t String $> ""
  asPair t   = do
    t1 <- fresh
    t2 <- fresh
    unify t (Var t1 :* Var t2) $> (Var t1, Var t2)

  index arr sub = do
    _ <- unify sub Int
    field <- fresh
    Var field <$ unify (Array (Var field)) arr

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')

  liftNumeric _ = unify (Int :+ Float :+ Rational)
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

  loop f = f empty
