{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Value.Type
  ( Type (..)
  , TypeError (..)
  , TypeMap
  , runTypes
  , runTypesWith
  , unify
  , runFunction
  , runBoolean
  , runWhile
  ) where

import           Control.Algebra
import           Control.Carrier.Resumable.Either (SomeError)
import qualified Control.Carrier.Resumable.Either as Either
import qualified Control.Carrier.Resumable.Resume as With
import           Control.Carrier.State.Strict
import           Control.Monad
import           Data.Functor
import           Data.Functor.Classes
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map

import           Control.Abstract hiding
    (Array (..), Boolean (..), Function (..), Hash (..), Numeric (..), Object (..), String (..), Unit (..), While (..), Void)
import qualified Control.Abstract as Abstract
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import           Data.Semigroup.Foldable (foldMap1)

type TName = Int

-- | A datatype representing primitive types and combinations thereof.
data Type
  = Int                 -- ^ Primitive int type.
  | Bool                -- ^ Primitive boolean type.
  | String              -- ^ Primitive string type.
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
  liftEq      _ (InfiniteType a1 b1) (InfiniteType a2 b2)         = a1 == a2 && b1 == b2
  liftEq      _ _ _                                               = False

instance Ord1  TypeError where
  liftCompare _ (UnificationError a1 b1) (UnificationError a2 b2) = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType a1 b1) (InfiniteType a2 b2)         = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType _ _) (UnificationError _ _)         = LT
  liftCompare _ (UnificationError _ _) (InfiniteType _ _)         = GT

instance Show1 TypeError where liftShowsPrec _ _ = showsPrec

runTypeError :: Evaluator term address value (Either.ResumableC (BaseError TypeError) m) a
             -> Evaluator term address value m (Either (SomeError (BaseError TypeError)) a)
runTypeError = raiseHandler Either.runResumable

runTypeErrorWith :: (forall resume . (BaseError TypeError) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (With.ResumableC (BaseError TypeError) m) a
                 -> Evaluator term address value m a
runTypeErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)


throwTypeError :: ( Has (Resumable (BaseError TypeError)) sig m
                  , Has (Reader ModuleInfo) sig m
                  , Has (Reader Span) sig m
                  )
               => TypeError resume
               -> m resume
throwTypeError = throwBaseError

runTypeMap :: Algebra sig m
           => Evaluator term address Type (StateC TypeMap m) a
           -> Evaluator term address Type m a
runTypeMap = raiseHandler $ fmap snd . runState emptyTypeMap

runTypes :: Algebra sig m
         => Evaluator term address Type (Either.ResumableC (BaseError TypeError)
                                         (StateC TypeMap m)) a
         -> Evaluator term address Type m (Either (SomeError (BaseError TypeError)) a)
runTypes = runTypeMap . runTypeError

runTypesWith :: Algebra sig m
             => (forall resume . (BaseError TypeError) resume -> Evaluator term address Type (StateC TypeMap m) resume)
             -> Evaluator term address Type (With.ResumableC (BaseError TypeError)
                                            (StateC TypeMap
                                            m)) a
             -> Evaluator term address Type m a
runTypesWith with = runTypeMap . runTypeErrorWith with

-- TODO: change my name?
newtype TypeMap = TypeMap { unTypeMap :: Map.Map TName Type }

emptyTypeMap :: TypeMap
emptyTypeMap = TypeMap Map.empty

modifyTypeMap :: Has (State TypeMap) sig m
              => (Map.Map TName Type -> Map.Map TName Type)
              -> m ()
modifyTypeMap f = modify (TypeMap . f . unTypeMap)

-- | Prunes substituted type variables
prune :: Has (State TypeMap) sig m
      => Type
      -> m Type
prune (Var id) = gets (Map.lookup id . unTypeMap) >>= \case
                    Just ty -> do
                      pruned <- prune ty
                      modifyTypeMap (Map.insert id pruned)
                      pure pruned
                    Nothing -> pure (Var id)
prune ty = pure ty

-- | Checks whether a type variable name occurs within another type. This
--   function is used in 'substitute' to prevent unification of infinite types
occur :: Has (State TypeMap) sig m
      => TName
      -> Type
      -> m Bool
occur id = prune >=> \case
  Int -> pure False
  Bool -> pure False
  String -> pure False
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
substitute :: ( Has (Reader ModuleInfo) sig m
              , Has (Reader Span) sig m
              , Has (Resumable (BaseError TypeError)) sig m
              , Has (State TypeMap) sig m
              )
           => TName
           -> Type
           -> m Type
substitute id ty = do
  infiniteType <- occur id ty
  ty <- if infiniteType
    then throwTypeError (InfiniteType (Var id) ty)
    else pure ty
  modifyTypeMap (Map.insert id ty)
  pure ty

-- | Unify two 'Type's.
unify :: ( Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         )
      => Type
      -> Type
      -> m Type
unify a b = do
  a' <- prune a
  b' <- prune b
  case (a', b') of
    (a1 :-> b1, a2 :-> b2) -> (:->) <$> unify a1 a2 <*> unify b1 b2
    (a, Null)              -> pure a
    (Null, b)              -> pure b
    (Var id, ty)           -> substitute id ty
    (ty, Var id)           -> substitute id ty
    (Array t1, Array t2)   -> Array <$> unify t1 t2
    -- FIXME: unifying with sums should distribute nondeterministically.
    -- FIXME: ordering shouldn’t be significant for undiscriminated sums.
    (a1 :+ b1, a2 :+ b2)   -> (:+) <$> unify a1 a2 <*> unify b1 b2
    (a1 :* b1, a2 :* b2)   -> (:*) <$> unify a1 a2 <*> unify b1 b2
    (t1, t2) | t1 == t2    -> pure t2
    _                      -> throwTypeError (UnificationError a b)

instance Ord address => ValueRoots address Type where
  valueRoots _ = mempty


instance ( Has (Allocator address) sig m
         , Has (Deref Type) sig m
         , Has (Error (Return Type)) sig m
         , Has Fresh sig m
         , Has (Reader (CurrentFrame address)) sig m
         , Has (Reader (CurrentScope address)) sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (State Span) sig m
         , Has (Resumable (BaseError (EvalError term address Type))) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (Resumable (BaseError (AddressError address Type))) sig m
         , Has (State (Heap address address Type)) sig m
         , Has (State (ScopeGraph address)) sig m
         , Has (Resumable (BaseError (ScopeError address))) sig m
         , Has (Resumable (BaseError (HeapError address))) sig m
         , Has (State TypeMap) sig m
         , Declarations term
         , Ord address
         , Show address
         , Algebra sig m
         )
      => Algebra (Abstract.Function term address Type :+: sig) (FunctionC term address Type m) where
  alg (R other) = FunctionC (alg (R (handleCoercible other)))
  alg (L op) = runEvaluator $ do
    eval <- Evaluator . FunctionC $ ask
    case op of
      Abstract.Function _ params body scope k -> do
        currentScope' <- currentScope
        currentFrame' <- currentFrame
        let frameLinks = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        frame <- newFrame scope frameLinks
        res <- withScopeAndFrame frame $ do
          tvars <- foldr (\ param rest -> do
            tvar <- Var <$> fresh
            slot <- lookupSlot (Declaration param)
            assign slot tvar
            (tvar :) <$> rest) (pure []) params
          -- TODO: We may still want to represent this as a closure and not a function type
          (zeroOrMoreProduct tvars :->) <$> catchReturn (Evaluator (eval body))
        Evaluator (k res)

      Abstract.BuiltIn _ Print k -> Evaluator $ k (String :-> Unit)
      Abstract.BuiltIn _ Show  k -> Evaluator $ k (Object :-> String)
      Abstract.Bind _ value k -> Evaluator $ k value
      Abstract.Call op paramTypes k -> do
        tvar <- fresh
        let needed = zeroOrMoreProduct paramTypes :-> Var tvar
        unified <- op `unify` needed
        boxed <- case unified of
          _ :-> ret -> pure ret
          actual    -> throwTypeError (UnificationError needed actual)
        Evaluator (k boxed)



instance ( Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         , Alternative m
         )
      => Algebra (Abstract.Boolean Type :+: sig) (BooleanC Type m) where
  alg (R other)                  = BooleanC . alg . handleCoercible $ other
  alg (L (Abstract.Boolean _ k)) = k Bool
  alg (L (Abstract.AsBool t k))  = unify t Bool *> (k True <|> k False)



instance ( Has (Abstract.Boolean Type) sig m
         , Algebra sig m
         , Alternative m
         )
      => Algebra (Abstract.While Type :+: sig) (WhileC Type m) where
  alg (R other) = WhileC . alg . handleCoercible $ other
  alg (L (Abstract.While cond body k)) = do
    cond' <- cond
    ifthenelse cond' (body *> empty) (k Unit)


instance Algebra sig m
      => Algebra (Abstract.Unit Type :+: sig) (UnitC Type m) where
  alg (R other)             = UnitC . alg . handleCoercible $ other
  alg (L (Abstract.Unit k)) = k Unit

instance ( Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         , Alternative m
         )
      => Algebra (Abstract.String Type :+: sig) (StringC Type m) where
  alg (R other)                   = StringC . alg . handleCoercible $ other
  alg (L (Abstract.String _ k))   = k String
  alg (L (Abstract.AsString t k)) = unify t String *> k ""

instance ( Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         , Monad m
         )
      => Algebra (Abstract.Numeric Type :+: sig) (NumericC Type m) where
  alg (R other) = NumericC . alg . handleCoercible $ other
  alg (L op) = case op of
    Abstract.Integer _ k -> k Int
    Abstract.Float _ k -> k Float
    Abstract.Rational _ k -> k Rational
    Abstract.LiftNumeric _ t k -> unify (Int :+ Float :+ Rational) t >>= k
    Abstract.LiftNumeric2 _ left right k -> case (left, right) of
      (Float, Int) -> k Float
      (Int, Float) -> k Float
      _            -> unify left right >>= k

instance ( Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         , Monad m
         )
      => Algebra (Abstract.Bitwise Type :+: sig) (BitwiseC Type m) where
  alg (R other) = BitwiseC . alg . handleCoercible $ other
  alg (L op) = case op of
    CastToInteger t k      -> unify t (Int :+ Float :+ Rational) *> k Int
    LiftBitwise _ t k      -> unify t Int >>= k
    LiftBitwise2 _ t1 t2 k -> unify Int t1 >>= unify t2 >>= k
    UnsignedRShift t1 t2 k -> unify Int t2 *> unify Int t1 >>= k

instance ( Algebra sig m ) => Algebra (Abstract.Object address Type :+: sig) (ObjectC address Type m) where
  alg (R other) = ObjectC . alg . handleCoercible $ other
  alg (L op) = case op of
    Abstract.Object _ k            -> k Object
    Abstract.ScopedEnvironment _ k -> k Nothing
    Abstract.Klass _ _ k           -> k Object

instance ( Has Fresh sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         , Monad m
         )
      => Algebra (Abstract.Array Type :+: sig) (ArrayC Type m) where
  alg (R other) = ArrayC . alg . handleCoercible $ other
  alg (L (Abstract.Array fieldTypes k)) = do
    var <- fresh
    fieldType <- foldr (\ t1 -> (unify t1 =<<)) (pure (Var var)) fieldTypes
    k (Array fieldType)
  alg (L (Abstract.AsArray t k)) = do
    field <- fresh
    unify t (Array (Var field)) >> k mempty

instance ( Algebra sig m ) => Algebra (Abstract.Hash Type :+: sig) (HashC Type m) where
  alg (R other)                     = HashC . alg . handleCoercible $ other
  alg (L (Abstract.Hash t k))       = k (Hash t)
  alg (L (Abstract.KvPair t1 t2 k)) = k (t1 :* t2)


instance AbstractHole Type where
  hole = Hole

instance AbstractIntro Type where
  null        = Null

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Has Fresh sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError TypeError)) sig m
         , Has (State TypeMap) sig m
         , Algebra sig m
         )
      => AbstractValue term address Type m where
  tuple fields = pure $ zeroOrMoreProduct fields

  namespace _ _   = pure Unit

  asPair t   = do
    t1 <- fresh
    t2 <- fresh
    unify t (Var t1 :* Var t2) $> (Var t1, Var t2)

  index arr sub = do
    _ <- unify sub Int
    field <- fresh
    _ <- unify (Array (Var field)) arr
    pure (Var field)

  liftComparison (Concrete _) left right = case (left, right) of
    (Float, Int) ->                     pure Bool
    (Int, Float) ->                     pure Bool
    _            -> unify left right $> Bool
  liftComparison Generalized left right = case (left, right) of
    (Float, Int) ->                     pure Int
    (Int, Float) ->                     pure Int
    _            -> unify left right $> Bool
