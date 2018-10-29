{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, LambdaCase #-}
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

import qualified Control.Abstract as Abstract
import Control.Abstract hiding (Boolean(..), Function(..), While(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Abstract.Environment as Env
import Data.Abstract.BaseError
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

runTypeError :: (Carrier sig m, Effect sig) => Evaluator term address value (ResumableC (BaseError TypeError) (Eff m)) a -> Evaluator term address value m (Either (SomeError (BaseError TypeError)) a)
runTypeError = raiseHandler runResumable

runTypeErrorWith :: Carrier sig m => (forall resume . (BaseError TypeError) resume -> Evaluator term address value m resume) -> Evaluator term address value (ResumableWithC (BaseError TypeError) (Eff m)) a -> Evaluator term address value m a
runTypeErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)


throwTypeError :: ( Member (Resumable (BaseError TypeError)) sig
                  , Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Carrier sig m
                  , Monad m
                  )
               => TypeError resume
               -> m resume
throwTypeError = throwBaseError

runTypeMap :: (Carrier sig m, Effect sig)
           => Evaluator term address Type (StateC TypeMap (Eff m)) a
           -> Evaluator term address Type m a
runTypeMap = raiseHandler $ fmap snd . runState emptyTypeMap

runTypes :: (Carrier sig m, Effect sig)
         => Evaluator term address Type (ResumableC (BaseError TypeError) (Eff
                                        (StateC TypeMap (Eff
                                        m)))) a
         -> Evaluator term address Type m (Either (SomeError (BaseError TypeError)) a)
runTypes = runTypeMap . runTypeError

runTypesWith :: (Carrier sig m, Effect sig)
             => (forall resume . (BaseError TypeError) resume -> Evaluator term address Type (StateC TypeMap (Eff m)) resume)
             -> Evaluator term address Type (ResumableWithC (BaseError TypeError) (Eff
                                            (StateC TypeMap (Eff
                                            m)))) a
             -> Evaluator term address Type m a
runTypesWith with = runTypeMap . runTypeErrorWith with

-- TODO: change my name?
newtype TypeMap = TypeMap { unTypeMap :: Map.Map TName Type }

emptyTypeMap :: TypeMap
emptyTypeMap = TypeMap Map.empty

modifyTypeMap :: ( Member (State TypeMap) sig
                 , Carrier sig m
                 , Monad m
                 )
              => (Map.Map TName Type -> Map.Map TName Type)
              -> m ()
modifyTypeMap f = modify (TypeMap . f . unTypeMap)

-- | Prunes substituted type variables
prune :: ( Member (State TypeMap) sig
         , Carrier sig m
         , Monad m
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
         , Monad m
         )
      => TName
      -> Type
      -> m Bool
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
substitute :: ( Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Member (Resumable (BaseError TypeError)) sig
              , Member (State TypeMap) sig
              , Carrier sig m
              , Monad m
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
         , Monad m
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
         , Member (Env address) sig
         , Member (Error (Return address)) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (Resumable (BaseError (AddressError address Type))) sig
         , Member (State (Heap address Type)) sig
         , Member (State TypeMap) sig
         , Ord address
         , Carrier sig m
         )
      => Carrier (Abstract.Function term address Type :+: sig) (FunctionC term address Type (Eff m)) where
  ret = FunctionC . const . ret
  eff op = FunctionC (\ eval -> handleSum (eff . handleReader eval runFunctionC) (\case
    Abstract.Function _ params body k -> runEvaluator $ do
      (env, tvars) <- foldr (\ name rest -> do
        addr <- alloc name
        tvar <- Var <$> fresh
        assign addr tvar
        bimap (Env.insert name addr) (tvar :) <$> rest) (pure (lowerBound, [])) params
      locally (catchReturn (bindAll env *> runFunction (Evaluator . eval) (Evaluator (eval body)))) >>= deref >>= Evaluator . flip runFunctionC eval . k . (zeroOrMoreProduct tvars :->)
    Abstract.BuiltIn Print k -> runFunctionC (k (String :-> Unit)) eval
    Abstract.BuiltIn Show  k -> runFunctionC (k (Object :-> String)) eval
    Abstract.Call op _ params k -> runEvaluator $ do
      tvar <- fresh
      paramTypes <- traverse deref params
      let needed = zeroOrMoreProduct paramTypes :-> Var tvar
      unified <- op `unify` needed
      boxed <- case unified of
        _ :-> ret -> box ret
        actual    -> throwTypeError (UnificationError needed actual) >>= box
      Evaluator $ runFunctionC (k boxed) eval) op)


instance ( Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State TypeMap) sig
         , Carrier sig m
         , Alternative m
         , Monad m
         )
      => Carrier (Abstract.Boolean Type :+: sig) (BooleanC Type m) where
  ret = BooleanC . ret
  eff = BooleanC . handleSum (eff . handleCoercible) (\case
    Abstract.Boolean _ k -> runBooleanC (k Bool)
    Abstract.AsBool  t k -> unify t Bool *> (runBooleanC (k True) <|> runBooleanC (k False)))


instance ( Member (Abstract.Boolean Type) sig
         , Carrier sig m
         , Alternative m
         , Monad m
         )
      => Carrier (Abstract.While Type :+: sig) (WhileC Type m) where
  ret = WhileC . ret
  eff = WhileC . handleSum
    (eff . handleCoercible)
    (\ (Abstract.While cond body k) -> do
      cond' <- runWhileC cond
      ifthenelse cond' (runWhileC body *> empty) (runWhileC (k unit)))


instance AbstractHole Type where
  hole = Hole

instance AbstractIntro Type where
  unit       = Unit
  integer _  = Int
  string _   = String
  float _    = Float
  symbol _   = Symbol
  regex _    = Regex
  rational _ = Rational
  hash       = Hash
  kvPair k v = k :* v

  null        = Null

-- | Discard the value arguments (if any), constructing a 'Type' instead.
instance ( Member (Allocator address) sig
         , Member (Deref Type) sig
         , Member Fresh sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address Type))) sig
         , Member (Resumable (BaseError TypeError)) sig
         , Member (State (Heap address Type)) sig
         , Member (State TypeMap) sig
         , Ord address
         , Carrier sig m
         )
      => AbstractValue term address Type m where
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
  asArray t = do
    field <- fresh
    unify t (Array (Var field)) $> mempty

  index arr sub = do
    _ <- unify sub Int
    field <- fresh
    _ <- unify (Array (Var field)) arr
    box (Var field)

  liftNumeric _ = unify (Int :+ Float :+ Rational)
  liftNumeric2 _ left right = case (left, right) of
    (Float, Int) -> pure Float
    (Int, Float) -> pure Float
    _            -> unify left right

  liftBitwise _ = unify Int
  liftBitwise2 _ t1 t2   = unify Int t1 >>= flip unify t2

  unsignedRShift t1 t2 = unify Int t2 *> unify Int t1

  liftComparison (Concrete _) left right = case (left, right) of
    (Float, Int) ->                     pure Bool
    (Int, Float) ->                     pure Bool
    _                 -> unify left right $> Bool
  liftComparison Generalized left right = case (left, right) of
    (Float, Int) ->                     pure Int
    (Int, Float) ->                     pure Int
    _                 -> unify left right $> Bool

  castToInteger t = unify t (Int :+ Float :+ Rational) $> Int
