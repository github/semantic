{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Symbolic where

import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Store
import Control.Applicative
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Abstract.Environment
import Data.Functor.Classes
import qualified Data.Set as Set
import Data.Term
import Data.Union

data Sym t a = Sym t | V a
  deriving (Eq, Ord, Show)

sym :: (Num a, Num t) => (forall n . Num n => n -> n) -> Sym t a -> Sym t a
sym f (Sym t) = Sym (f t)
sym f (V a) = V (f a)

sym2 :: Applicative f => (a -> a -> f a) -> (a -> t) -> (t -> t -> t) -> Sym t a -> Sym t a -> f (Sym t a)
sym2 f _ _ (V a) (V b) = V <$> f a b
sym2 _ _ g (Sym a) (Sym b) = pure (Sym (g a b))
sym2 f num g a (V b) = sym2 f num g a (Sym (num b))
sym2 f num g (V a) b = sym2 f num g (Sym (num a)) b


evSymbolic :: (Eval' t (Eff fs (v (Sym t a))) -> Eval' t (Eff fs (v (Sym t a))))
           -> Eval' t (Eff fs (v (Sym t a)))
           -> Eval' t (Eff fs (v (Sym t a)))
evSymbolic ev0 ev e = ev0 ev e


data PathExpression t = E t | NotE t
  deriving (Eq, Ord, Show)

newtype PathCondition t = PathCondition { unPathCondition :: Set.Set (PathExpression t) }
  deriving (Eq, Ord, Show)


refine :: (Ord t, MonadPathCondition t m) => PathExpression t -> m ()
refine = modifyPathCondition . pathConditionInsert

pathConditionMember :: Ord t => PathExpression t -> PathCondition t -> Bool
pathConditionMember = (. unPathCondition) . Set.member

pathConditionInsert :: Ord t => PathExpression t -> PathCondition t -> PathCondition t
pathConditionInsert = ((PathCondition .) . (. unPathCondition)) . Set.insert


class Monad m => MonadPathCondition t m where
  getPathCondition :: m (PathCondition t)
  putPathCondition :: PathCondition t -> m ()

instance (State (PathCondition t) :< fs) => MonadPathCondition t (Eff fs) where
  getPathCondition = get
  putPathCondition = put

modifyPathCondition :: MonadPathCondition t m => (PathCondition t -> PathCondition t) -> m ()
modifyPathCondition f = getPathCondition >>= putPathCondition . f

instance ( Alternative m
         , MonadFail m
         , MonadPrim Prim m
         , MonadPathCondition (Term (Union fs)) m
         , Apply Eq1 fs
         , Apply Ord1 fs
         , Binary :< fs
         , Unary :< fs
         , Primitive :< fs
         ) => MonadPrim (Sym (Term (Union fs)) Prim) m where
  delta1 o a = case o of
    Negate -> pure (negate a)
    Abs    -> pure (abs a)
    Signum -> pure (signum a)
    Not    -> case a of
      Sym t -> pure (Sym (not' t))
      V a -> V <$> delta1 Not a

  delta2 o a b = case o of
    Plus  -> pure (a + b)
    Minus -> pure (a - b)
    Times -> pure (a * b)
    DividedBy -> isZero b >>= flip when divisionByZero >> sym2 (delta2 DividedBy) prim div'  a b
    Quotient  -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Quotient)  prim quot' a b
    Remainder -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Remainder) prim rem'  a b
    Modulus   -> isZero b >>= flip when divisionByZero >> sym2 (delta2 Modulus)   prim mod'  a b
    And -> sym2 (delta2 And) prim and' a b
    Or  -> sym2 (delta2 Or)  prim or'  a b
    Eq  -> sym2 (delta2 Eq)  prim eq   a b
    Lt  -> sym2 (delta2 Lt)  prim lt   a b
    LtE -> sym2 (delta2 LtE) prim lte  a b
    Gt  -> sym2 (delta2 Gt)  prim gt   a b
    GtE -> sym2 (delta2 GtE) prim gte  a b

  truthy (V a) = truthy a
  truthy (Sym e) = do
    phi <- getPathCondition
    if E e `pathConditionMember` phi then
      return True
    else if NotE e `pathConditionMember` phi then
      return False
    else
         (refine (E e)    >> return True)
     <|> (refine (NotE e) >> return False)

instance (Binary :< fs, Unary :< fs, Primitive :< fs) => Num (Sym (Term (Union fs)) Prim) where
  fromInteger = V . fromInteger

  signum (V a)   = V   (signum a)
  signum (Sym t) = Sym (signum t)
  abs (V a)      = V   (abs    a)
  abs (Sym t)    = Sym (abs    t)
  negate (V a)   = V   (negate a)
  negate (Sym t) = Sym (negate t)
  V   a + V   b = V   (     a +      b)
  Sym a + V   b = Sym (     a + prim b)
  V   a + Sym b = Sym (prim a +      b)
  Sym a + Sym b = Sym (     a +      b)
  V   a - V   b = V   (     a -      b)
  Sym a - V   b = Sym (     a - prim b)
  V   a - Sym b = Sym (prim a -      b)
  Sym a - Sym b = Sym (     a -      b)
  V   a * V   b = V   (     a *      b)
  Sym a * V   b = Sym (     a * prim b)
  V   a * Sym b = Sym (prim a *      b)
  Sym a * Sym b = Sym (     a *      b)
