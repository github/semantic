{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
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

import Control.Abstract.ScopeGraph
import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..), Numeric(..), Object(..), Array(..), Hash(..), String(..), Unit(..), While(..))
import Control.Effect.Carrier
import Data.Abstract.BaseError
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Map as Map
import Prologue hiding (TypeError)
import Data.Abstract.Evaluatable

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
  liftEq      _ (InfiniteType a1 b1) (InfiniteType a2 b2) = a1 == a2 && b1 == b2
  liftEq      _ _ _ = False

instance Ord1  TypeError where
  liftCompare _ (UnificationError a1 b1) (UnificationError a2 b2) = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType a1 b1) (InfiniteType a2 b2) = compare a1 a2 <> compare b1 b2
  liftCompare _ (InfiniteType _ _) (UnificationError _ _) = LT
  liftCompare _ (UnificationError _ _) (InfiniteType _ _) = GT

instance Show1 TypeError where liftShowsPrec _ _ = showsPrec

runTypeError :: Evaluator term address value (ResumableC (BaseError TypeError) m) a
             -> Evaluator term address value m (Either (SomeError (BaseError TypeError)) a)
runTypeError = raiseHandler runResumable

runTypeErrorWith :: (forall resume . (BaseError TypeError) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (ResumableWithC (BaseError TypeError) m) a
                 -> Evaluator term address value m a
runTypeErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)


throwTypeError :: ( Member (Resumable (BaseError TypeError)) sig
                  , Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Carrier sig m
                  )
               => TypeError resume
               -> m resume
throwTypeError = throwBaseError

runTypeMap :: Carrier sig m
           => Evaluator term address Type (StateC TypeMap m) a
           -> Evaluator term address Type m a
runTypeMap = raiseHandler $ fmap snd . runState emptyTypeMap

runTypes :: Carrier sig m
         => Evaluator term address Type (ResumableC (BaseError TypeError)
                                         (StateC TypeMap m)) a
         -> Evaluator term address Type m (Either (SomeError (BaseError TypeError)) a)
runTypes = runTypeMap . runTypeError

runTypesWith :: Carrier sig m
             => (forall resume . (BaseError TypeError) resume -> Evaluator term address Type (StateC TypeMap m) resume)
             -> Evaluator term address Type (ResumableWithC (BaseError TypeError)
                                            (StateC TypeMap
                                            m)) a
             -> Evaluator term address Type m a
runTypesWith with = runTypeMap . runTypeErrorWith with

-- TODO: change my name?
newtype TypeMap = TypeMap { unTypeMap :: Map.Map TName Type }

emptyTypeMap :: TypeMap
emptyTypeMap = TypeMap Map.empty

modifyTypeMap :: ( Member (State TypeMap) sig
                 , Carrier sig m
                 )
              => (Map.Map TName Type -> Map.Map TName Type)
              -> m ()
modifyTypeMap f = modify (TypeMap . f . unTypeMap)

-- | Prunes substituted type variables
prune :: ( Member (State TypeMap) sig
         , Carrier sig m
         )
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
occur :: ( Member (State TypeMap) sig
         , Carrier sig m
         )
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
substitute :: ( Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Member (Resumable (BaseError TypeError)) sig
              , Member (State TypeMap) sig
              , Carrier sig m
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
unify :: ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         )
      => Type
      -> Type
      -> m Type
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
    _ -> throwTypeError (UnificationError a b)

instance Ord address => ValueRoots address Type where
  valueRoots _ = mempty


instance ( Member (Allocator address) sig
         , Member (Deref Type) sig
         , Member (Error (Return Type)) sig
         , Member Fresh sig
         , Member (Reader (CurrentFrame address)) sig
         , Member (Reader (CurrentScope address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (State Span) sig
         , Member (Resumable (BaseError (EvalError term address Type))) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (Resumable (BaseError (AddressError address Type))) sig
         , Member (State (Heap address address Type)) sig
         , Member (State (ScopeGraph address)) sig
         , Member (Resumable (BaseError (ScopeError address))) sig
         , Member (Resumable (BaseError (HeapError address))) sig
         , Member (State TypeMap) sig
         , Declarations term
         , Ord address
         , Show address
         , Carrier sig m
         )
      => Carrier (Abstract.Function term address Type :+: sig) (FunctionC term address Type m) where
  eff (R other) = FunctionC (eff (R (handleCoercible other)))
  eff (L op) = runEvaluator $ do
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



instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Alternative m
         )
      => Carrier (Abstract.Boolean Type :+: sig) (BooleanC Type m) where
  eff (R other) = BooleanC . eff . handleCoercible $ other
  eff (L (Abstract.Boolean _ k)) = k Bool
  eff (L (Abstract.AsBool t k))  = unify t Bool *> (k True <|> k False)



instance ( Member (Abstract.Boolean Type) sig
         , Carrier sig m
         , Alternative m
         )
      => Carrier (Abstract.While Type :+: sig) (WhileC Type m) where
  eff (R other) = WhileC . eff . handleCoercible $ other
  eff (L (Abstract.While cond body k)) = do
    cond' <- cond
    ifthenelse cond' (body *> empty) (k Unit)


instance Carrier sig m
      => Carrier (Abstract.Unit Type :+: sig) (UnitC Type m) where
  eff (R other) = UnitC . eff . handleCoercible $ other
  eff (L (Abstract.Unit k)) = k Unit

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Alternative m
         )
      => Carrier (Abstract.String Type :+: sig) (StringC Type m) where
  eff (R other) = StringC . eff . handleCoercible $ other
  eff (L (Abstract.String _ k)) = k String
  eff (L (Abstract.AsString t k)) = unify t String *> k ""

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Numeric Type :+: sig) (NumericC Type m) where
  eff (R other) = NumericC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Integer _ k -> k Int
    Abstract.Float _ k -> k Float
    Abstract.Rational _ k -> k Rational
    Abstract.LiftNumeric _ t k -> unify (Int :+ Float :+ Rational) t >>= k
    Abstract.LiftNumeric2 _ left right k -> case (left, right) of
      (Float, Int) -> k Float
      (Int, Float) -> k Float
      _            -> unify left right >>= k

instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Bitwise Type :+: sig) (BitwiseC Type m) where
  eff (R other) = BitwiseC . eff . handleCoercible $ other
  eff (L op) = case op of
    CastToInteger t k -> unify t (Int :+ Float :+ Rational) *> k Int
    LiftBitwise _ t k -> unify t Int >>= k
    LiftBitwise2 _ t1 t2 k -> unify Int t1 >>= unify t2 >>= k
    UnsignedRShift t1 t2 k -> unify Int t2 *> unify Int t1 >>= k

instance ( Carrier sig m ) => Carrier (Abstract.Object address Type :+: sig) (ObjectC address Type m) where
  eff (R other) = ObjectC . eff . handleCoercible $ other
  eff (L op) = case op of
    Abstract.Object _ k -> k Object
    Abstract.ScopedEnvironment _ k -> k Nothing
    Abstract.Klass _ _ k -> k Object

instance ( Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Monad m
         )
      => Carrier (Abstract.Array Type :+: sig) (ArrayC Type m) where
  eff (R other) = ArrayC . eff . handleCoercible $ other
  eff (L (Abstract.Array fieldTypes k)) = do
    var <- fresh
    fieldType <- foldr (\ t1 -> (unify t1 =<<)) (pure (Var var)) fieldTypes
    k (Array fieldType)
  eff (L (Abstract.AsArray t k)) = do
    field <- fresh
    unify t (Array (Var field)) >> k mempty

instance ( Carrier sig m ) => Carrier (Abstract.Hash Type :+: sig) (HashC Type m) where
  eff (R other) = HashC . eff . handleCoercible $ other
  eff (L (Abstract.Hash t k)) = k (Hash t)
  eff (L (Abstract.KvPair t1 t2 k)) = k (t1 :* t2)


instance AbstractHole Type where
  hole = Hole

instance AbstractIntro Type where
  null        = Null

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
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
    _                 -> unify left right $> Bool
  liftComparison Generalized left right = case (left, right) of
    (Float, Int) ->                     pure Int
    (Int, Float) ->                     pure Int
    _                 -> unify left right $> Bool
