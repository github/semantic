{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, LambdaCase #-}
module Data.Abstract.Value.Type
  ( Type (..)
  , TypeError (..)
  , TypeMap
  , runTypes
  , runTypesWith
  , unify
  , runFunction
  ) where

import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Function(..), raiseHandler)
import Control.Monad.Effect.Internal (raiseHandler)
import Data.Abstract.Environment as Env
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Map as Map
import Prologue hiding (TypeError)

type TName = Int

-- | A datatype representing primitive types and combinations thereof.
data Type
  = Int                 -- ^ Primitive int type.
  | Bool                -- ^ Primitive boolean type.
  | String              -- ^ Primitive string type.
  | Symbol              -- ^ Type of unique symbols.
  | Regex               -- ^ Primitive regex type.
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
  InfiniteType :: Type -> Type -> TypeError Type

deriving instance Eq   (TypeError resume)
deriving instance Ord  (TypeError resume)
deriving instance Show (TypeError resume)

instance Eq1   TypeError where
  liftEq      _ (UnificationError a1 b1) (UnificationError a2 b2) = a1 == a2 && b1 == b2
  liftEq      _ (InfiniteType a1 b1) (InfiniteType a2 b2) = a1 == a2 && b1 == b2
  liftEq      _ _ _ = False

instance Ord1  TypeError where
  liftCompare _ (UnificationError a1 b1) (UnificationError a2 b2) = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType a1 b1) (InfiniteType a2 b2) = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType _ _) (UnificationError _ _) = LT
  liftCompare _ (UnificationError _ _) (InfiniteType _ _) = GT

instance Show1 TypeError where liftShowsPrec _ _ = showsPrec

runTypeError :: (Effectful m, Effects effects) => m (Resumable TypeError ': effects) a -> m effects (Either (SomeExc TypeError) a)
runTypeError = runResumable

runTypeErrorWith :: (Effectful m, Effects effects) => (forall resume . TypeError resume -> m effects resume) -> m (Resumable TypeError ': effects) a -> m effects a
runTypeErrorWith = runResumableWith

runTypeMap :: ( Effectful m
              , Effects effects
              )
           => m (State TypeMap ': effects) a
           -> m effects a
runTypeMap = raiseHandler (runState emptyTypeMap >=> pure . snd)

runTypes :: ( Effectful m
            , Effects effects
            )
         => m (Resumable TypeError ': State TypeMap ': effects) a
         -> m effects (Either (SomeExc TypeError) a)
runTypes = runTypeMap . runTypeError

runTypesWith :: ( Effectful m
                , Effects effects
                )
             => (forall resume . TypeError resume -> m (State TypeMap ': effects) resume)
             -> m (Resumable TypeError ': State TypeMap ': effects) a
             -> m effects a
runTypesWith with = runTypeMap . runTypeErrorWith with

-- TODO: change my name?
newtype TypeMap = TypeMap { unTypeMap :: Map.Map TName Type }

emptyTypeMap :: TypeMap
emptyTypeMap = TypeMap Map.empty

modifyTypeMap :: ( Effectful m
                 , Member (State TypeMap) effects
                 )
              => (Map.Map TName Type -> Map.Map TName Type)
              -> m effects ()
modifyTypeMap f = modify (TypeMap . f . unTypeMap)

-- | Prunes substituted type variables
prune :: ( Effectful m
         , Monad (m effects)
         , Member (State TypeMap) effects
         )
      => Type
      -> m effects Type
prune (Var id) = Map.lookup id . unTypeMap <$> get >>= \case
                    Just ty -> do
                      pruned <- prune ty
                      modifyTypeMap (Map.insert id pruned)
                      pure pruned
                    Nothing -> pure (Var id)
prune ty = pure ty

-- | Checks whether a type variable name occurs within another type. This
--   function is used in 'substitute' to prevent unification of infinite types
occur :: ( Effectful m
         , Monad (m effects)
         , Member (State TypeMap) effects
         )
      => TName
      -> Type
      -> m effects Bool
occur id = prune >=> \case
  Int -> pure False
  Bool -> pure False
  String -> pure False
  Symbol -> pure False
  Regex -> pure False
  Unit -> pure False
  Float -> pure False
  Rational -> pure False
  Void -> pure False
  Object -> pure False
  Null -> pure False
  Hole -> pure False
  a :-> b -> eitherM (occur id) (a, b)
  a :* b -> eitherM (occur id) (a, b)
  a :+ b -> eitherM (occur id) (a, b)
  Array ty -> occur id ty
  Hash kvs -> or <$> traverse (eitherM (occur id)) kvs
  Var vid -> pure (vid == id)
  where
    eitherM :: Applicative m => (a -> m Bool) -> (a, a) -> m Bool
    eitherM f (a, b) = (||) <$> f a <*> f b

-- | Substitutes a type variable name for another type
substitute :: ( Effectful m
              , Monad (m effects)
              , Member (Resumable TypeError) effects
              , Member (State TypeMap) effects
              )
           => TName
           -> Type
           -> m effects Type
substitute id ty = do
  infiniteType <- occur id ty
  ty <- if infiniteType
    then throwResumable (InfiniteType (Var id) ty)
    else pure ty
  modifyTypeMap (Map.insert id ty)
  pure ty

-- | Unify two 'Type's.
unify :: ( Effectful m
         , Monad (m effects)
         , Member (Resumable TypeError) effects
         , Member (State TypeMap) effects
         )
      => Type
      -> Type
      -> m effects Type
unify a b = do
  a' <- prune a
  b' <- prune b
  case (a', b') of
    (a1 :-> b1, a2 :-> b2) -> (:->) <$> unify a1 a2 <*> unify b1 b2
    (a, Null) -> pure a
    (Null, b) -> pure b
    (Var id, ty) -> substitute id ty
    (ty, Var id) -> substitute id ty
    (Array t1, Array t2) -> Array <$> unify t1 t2
    -- FIXME: unifying with sums should distribute nondeterministically.
    -- FIXME: ordering shouldn’t be significant for undiscriminated sums.
    (a1 :+ b1, a2 :+ b2) -> (:+) <$> unify a1 a2 <*> unify b1 b2
    (a1 :* b1, a2 :* b2) -> (:*) <$> unify a1 a2 <*> unify b1 b2
    (t1, t2) | t1 == t2 -> pure t2
    _ -> throwResumable (UnificationError a b)

instance Ord address => ValueRoots address Type where
  valueRoots _ = mempty


runFunction :: ( Member (Allocator address Type) effects
               , Member (Deref address Type) effects
               , Member (Env address) effects
               , Member (Exc (Return address)) effects
               , Member Fresh effects
               , Member (Resumable TypeError) effects
               , Member (State TypeMap) effects
               , PureEffects effects
               )
            => Evaluator address Type (Abstract.Function address Type ': effects) a
            -> Evaluator address Type effects a
runFunction = interpret $ \case
  Abstract.Function params _ body -> do
    (env, tvars) <- foldr (\ name rest -> do
      addr <- alloc name
      tvar <- Var <$> fresh
      assign addr tvar
      bimap (Env.insert name addr) (tvar :) <$> rest) (pure (lowerBound, [])) params
    (zeroOrMoreProduct tvars :->) <$> (locally (catchReturn (bindAll env *> runFunction (Evaluator body))) >>= deref)
  Abstract.Call op _ params -> do
    tvar <- fresh
    paramTypes <- traverse deref params
    let needed = zeroOrMoreProduct paramTypes :-> Var tvar
    unified <- op `unify` needed
    case unified of
      _ :-> ret -> box ret
      actual    -> throwResumable (UnificationError needed actual) >>= box


instance AbstractHole Type where
  hole = Hole

instance AbstractIntro Type where
  unit       = Unit
  integer _  = Int
  boolean _  = Bool
  string _   = String
  float _    = Float
  symbol _   = Symbol
  regex _    = Regex
  rational _ = Rational
  hash       = Hash
  kvPair k v = k :* v

  null        = Null

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Member (Allocator address Type) effects
         , Member (Deref address Type) effects
         , Member Fresh effects
         , Member NonDet effects
         , Member (Resumable TypeError) effects
         , Member (State TypeMap) effects
         )
      => AbstractValue address Type effects where
  array fields = do
    var <- fresh
    fieldTypes <- traverse deref fields
    Array <$> foldr (\ t1 -> (unify t1 =<<)) (pure (Var var)) fieldTypes

  tuple fields = zeroOrMoreProduct <$> traverse deref fields

  klass _ _ _     = pure Object
  namespace _ _ _ = pure Unit

  scopedEnvironment _ = pure (Just lowerBound)

  asString t = unify t String $> ""
  asPair t   = do
    t1 <- fresh
    t2 <- fresh
    unify t (Var t1 :* Var t2) $> (Var t1, Var t2)

  index arr sub = do
    _ <- unify sub Int
    field <- fresh
    _ <- unify (Array (Var field)) arr
    box (Var field)

  ifthenelse cond if' else' = unify cond Bool *> (if' <|> else')
  disjunction a b = do
    a' <- a
    unify a' Bool *> (pure a' <|> b)

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
