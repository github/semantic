{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Expression where

import Prelude hiding (null)
import Prologue hiding (This, index, null)

import Data.Fixed
import Data.List (intersperse)
import Proto3.Suite.Class

import           Control.Abstract hiding (Call, Member, Void)
import           Data.Abstract.Evaluatable as Abstract hiding (Member, Void)
import           Data.Abstract.Name as Name
import           Data.Abstract.Number (liftIntegralFrac, liftReal, liftedExponent, liftedFloorDiv)
import           Data.JSON.Fields
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import qualified Data.Reprinting.Token as Token
import           Diffing.Algorithm hiding (Delete)
import           Reprinting.Tokenize hiding (Superclass)

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callContext :: ![a], callFunction :: !a, callParams :: ![a], callBlock :: !a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Declarations1 Call where
  liftDeclaredName declaredName Call{..} = declaredName callFunction

instance Eq1 Call where liftEq = genericLiftEq
instance Ord1 Call where liftCompare = genericLiftCompare
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Call where
  eval eval _ Call{..} = do
    op <- eval callFunction
    args <- traverse eval callParams
    call op args

instance Tokenize Call where
  tokenize Call{..} = within Scope.Call $ do
    -- TODO: callContext
    callFunction
    within' Scope.Params $ sequenceA_ (sep callParams)
    callBlock

data LessThan a = LessThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LessThan where liftEq = genericLiftEq
instance Ord1 LessThan where liftCompare = genericLiftCompare
instance Show1 LessThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThan where
  eval eval _ t = traverse eval t >>= go where
    go (LessThan a b) = liftComparison (Concrete (<)) a b

instance Tokenize LessThan where
  tokenize LessThan{..} = within' (Scope.InfixL (Compare Less) 4) $ lhs *> yield Token.Sym <* rhs

data LessThanEqual a = LessThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LessThanEqual where liftEq = genericLiftEq
instance Ord1 LessThanEqual where liftCompare = genericLiftCompare
instance Show1 LessThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (LessThanEqual a b) = liftComparison (Concrete (<=)) a b

instance Tokenize LessThanEqual where
  tokenize LessThanEqual{..} = within' (Scope.InfixL (CompareEql Less) 4) $ lhs *> yield Token.Sym <* rhs

data GreaterThan a = GreaterThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 GreaterThan where liftEq = genericLiftEq
instance Ord1 GreaterThan where liftCompare = genericLiftCompare
instance Show1 GreaterThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThan where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThan a b) = liftComparison (Concrete (>)) a b

instance Tokenize GreaterThan where
  tokenize GreaterThan{..} = within' (Scope.InfixL (Compare Greater) 4) $ lhs *> yield Token.Sym <* rhs

data GreaterThanEqual a = GreaterThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 GreaterThanEqual where liftEq = genericLiftEq
instance Ord1 GreaterThanEqual where liftCompare = genericLiftCompare
instance Show1 GreaterThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThanEqual a b) = liftComparison (Concrete (>=)) a b

instance Tokenize GreaterThanEqual where
  tokenize GreaterThanEqual{..} = within' (Scope.InfixL (CompareEql Greater) 4) $ lhs *> yield Token.Sym <* rhs

data Equal a = Equal { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Equal where liftEq = genericLiftEq
instance Ord1 Equal where liftCompare = genericLiftCompare
instance Show1 Equal where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Equal where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (Equal a b) = liftComparison (Concrete (==)) a b

instance Tokenize Equal where
  tokenize Equal{..} = within' (Scope.InfixL Eql 4) $ lhs *> yield Token.Sym <* rhs

data StrictEqual a = StrictEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 StrictEqual where liftEq = genericLiftEq
instance Ord1 StrictEqual where liftCompare = genericLiftCompare
instance Show1 StrictEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictEqual where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (StrictEqual a b) = liftComparison (Concrete (==)) a b

instance Tokenize StrictEqual where
  tokenize StrictEqual{..} = within' (Scope.InfixL StrictEql 4) $ lhs *> yield Token.Sym <* rhs

data Comparison a = Comparison { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Comparison where liftEq = genericLiftEq
instance Ord1 Comparison where liftCompare = genericLiftCompare
instance Show1 Comparison where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comparison where
  eval eval _ t = traverse eval t >>= go where
    go (Comparison a b) = liftComparison (Concrete (==)) a b

instance Tokenize Comparison where
  tokenize Comparison{..} = within' (Scope.InfixL Spaceship 4) $ lhs *> yield Token.Sym <* rhs

data Plus a = Plus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Plus where liftEq = genericLiftEq
instance Ord1 Plus where liftCompare = genericLiftCompare
instance Show1 Plus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Plus where
  eval eval _ t = traverse eval t >>= go where
    go (Plus a b) = liftNumeric2 add a b  where add    = liftReal (+)

instance Tokenize Plus where
  tokenize Plus{..} = within' (Scope.InfixL Add 6) $ lhs *> yield Token.Sym <* rhs

data Minus a = Minus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Minus where liftEq = genericLiftEq
instance Ord1 Minus where liftCompare = genericLiftCompare
instance Show1 Minus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Minus where
  eval eval _ t = traverse eval t >>= go where
    go (Minus a b) = liftNumeric2 (liftReal (-)) a b

instance Tokenize Minus where
  tokenize Minus{..} = within' (Scope.InfixL Subtract 6) $ lhs *> yield Token.Sym <* rhs

data Times a = Times { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Times where liftEq = genericLiftEq
instance Ord1 Times where liftCompare = genericLiftCompare
instance Show1 Times where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Times where
  eval eval _ t = traverse eval t >>= go where
    go (Times a b) = liftNumeric2 (liftReal (*)) a b

instance Tokenize Times where
  tokenize Times{..} = within' (Scope.InfixL Multiply 7) $ lhs *> yield Token.Sym <* rhs

data DividedBy a = DividedBy { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 DividedBy where liftEq = genericLiftEq
instance Ord1 DividedBy where liftCompare = genericLiftCompare
instance Show1 DividedBy where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DividedBy where
  eval eval _ t = traverse eval t >>= go where
    go (DividedBy a b) = liftNumeric2 (liftIntegralFrac div (/)) a b

instance Tokenize DividedBy where
  tokenize DividedBy{..} = within' (Scope.InfixL Divide 7) $ lhs *> yield Token.Sym <* rhs

data Modulo a = Modulo { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Modulo where liftEq = genericLiftEq
instance Ord1 Modulo where liftCompare = genericLiftCompare
instance Show1 Modulo where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Modulo where
  eval eval _ t = traverse eval t >>= go where
    go (Modulo a b) = liftNumeric2 (liftIntegralFrac mod mod') a b

instance Tokenize Modulo where
  tokenize Modulo{..} = within' (Scope.InfixL Modulus 7) $ lhs *> yield Token.Sym <* rhs

data Power a = Power { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Power where liftEq = genericLiftEq
instance Ord1 Power where liftCompare = genericLiftCompare
instance Show1 Power where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Power where
  eval eval _ t = traverse eval t >>= go where
    go (Power a b) = liftNumeric2 liftedExponent a b

instance Tokenize Power where
  tokenize Power{..} = within' (Scope.InfixL Raise 9) $ lhs *> yield Token.Sym <* rhs

newtype Negate a = Negate { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Negate where liftEq = genericLiftEq
instance Ord1 Negate where liftCompare = genericLiftCompare
instance Show1 Negate where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Negate where
  eval eval _ t = traverse eval t >>= go where
    go (Negate a) = liftNumeric negate a

instance Tokenize Negate where
  tokenize Negate{..} = within' (Scope.Prefix NumericNegate) $ yield Token.Sym <* value

data FloorDivision a = FloorDivision { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FloorDivision where liftEq = genericLiftEq
instance Ord1 FloorDivision where liftCompare = genericLiftCompare
instance Show1 FloorDivision where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FloorDivision where
  eval eval _ t = traverse eval t >>= go where
    go (FloorDivision a b) = liftNumeric2 liftedFloorDiv a b

instance Tokenize FloorDivision where
  tokenize FloorDivision{..} = within' (Scope.InfixL FloorDivide 7) $ lhs *> yield Token.Sym <* rhs

-- | Regex matching operators (Ruby's =~ and ~!)
data Matches a = Matches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Matches where liftEq = genericLiftEq
instance Ord1 Matches where liftCompare = genericLiftCompare
instance Show1 Matches where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Matches

instance Tokenize Matches where
  tokenize Matches{..} = within' (Scope.InfixL RegexMatch 1) $ lhs *> yield Token.Sym <* rhs

data NotMatches a = NotMatches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 NotMatches where liftEq = genericLiftEq
instance Ord1 NotMatches where liftCompare = genericLiftCompare
instance Show1 NotMatches where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NotMatches

instance Tokenize NotMatches where
  tokenize NotMatches{..} = within' (Scope.InfixL RegexNotMatch 1) $ lhs *> yield Token.Sym <* rhs

data Or a = Or { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Or where liftEq = genericLiftEq
instance Ord1 Or where liftCompare = genericLiftCompare
instance Show1 Or where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Or where
  eval eval _ (Or a b) = do
    a' <- eval a
    ifthenelse a' (pure a') (eval b)

instance Tokenize Or where
  tokenize Or{..} = within' (Scope.InfixL LogicalOr 2) $ lhs *> yield Token.Sym <* rhs

data And a = And { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 And where liftEq = genericLiftEq
instance Ord1 And where liftCompare = genericLiftCompare
instance Show1 And where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable And where
  eval eval _ (And a b) = do
    a' <- eval a
    ifthenelse a' (eval b) (pure a')

instance Tokenize And where
  tokenize And{..} = within' (Scope.InfixL LogicalAnd 2) $ lhs *> yield Token.Sym <* rhs

newtype Not a = Not { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Not where liftEq = genericLiftEq
instance Ord1 Not where liftCompare = genericLiftCompare
instance Show1 Not where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Not where
  eval eval _ (Not a) = eval a >>= asBool >>= boolean . not

instance Tokenize Not where
  tokenize Not{..} = within' (Scope.Prefix LogicalNot) $ yield Token.Sym <* value

data XOr a = XOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 XOr where liftEq = genericLiftEq
instance Ord1 XOr where liftCompare = genericLiftCompare
instance Show1 XOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable XOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ (XOr a b) = liftA2 (/=) (eval a >>= asBool) (eval b >>= asBool) >>= boolean

instance Tokenize XOr where
  tokenize XOr{..} = within' (Scope.InfixL LogicalXor 2) $ lhs *> yield Token.Sym <* rhs

-- | Javascript delete operator
newtype Delete a = Delete { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Delete where liftEq = genericLiftEq
instance Ord1 Delete where liftCompare = genericLiftCompare
instance Show1 Delete where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Delete where
  eval _ ref (Delete a) = ref a >>= dealloc >> pure unit

-- | A sequence expression such as Javascript or C's comma operator.
data SequenceExpression a = SequenceExpression { firstExpression :: !a, secondExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 SequenceExpression where liftEq = genericLiftEq
instance Ord1 SequenceExpression where liftCompare = genericLiftCompare
instance Show1 SequenceExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SequenceExpression where
  eval eval _ (SequenceExpression a b) =
    eval a >> eval b

-- | Javascript void operator
newtype Void a = Void { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Void where liftEq = genericLiftEq
instance Ord1 Void where liftCompare = genericLiftCompare
instance Show1 Void where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Void where
  eval eval _ (Void a) =
    eval a >> pure null

-- | Javascript typeof operator
newtype Typeof a = Typeof { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Typeof where liftEq = genericLiftEq
instance Ord1 Typeof where liftCompare = genericLiftCompare
instance Show1 Typeof where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Typeof
instance Evaluatable Typeof

-- | Bitwise operators.
data BOr a = BOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 BOr where liftEq = genericLiftEq
instance Ord1 BOr where liftCompare = genericLiftCompare
instance Show1 BOr where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BOr where
  eval eval _ (BOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.|.) a' b'

instance Tokenize BOr where
  tokenize BOr{..} = within' (Scope.InfixL BinaryOr 4) $ lhs *> yield Token.Sym <* rhs

data BAnd a = BAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 BAnd where liftEq = genericLiftEq
instance Ord1 BAnd where liftCompare = genericLiftCompare
instance Show1 BAnd where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BAnd where
  eval eval _ (BAnd a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.&.) a' b'

instance Tokenize BAnd where
  tokenize BAnd{..} = within' (Scope.InfixL BinaryAnd 5) $ lhs *> yield Token.Sym <* rhs

data BXOr a = BXOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 BXOr where liftEq = genericLiftEq
instance Ord1 BXOr where liftCompare = genericLiftCompare
instance Show1 BXOr where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BXOr where
  eval eval _ (BXOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 xor a' b'

instance Tokenize BXOr where
  tokenize BXOr{..} = within' (Scope.InfixL BinaryXor 5) $ lhs *> yield Token.Sym <* rhs

data LShift a = LShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LShift where liftEq = genericLiftEq
instance Ord1 LShift where liftCompare = genericLiftCompare
instance Show1 LShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LShift where
  eval eval _ (LShift a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 shiftL' a' b'
    where
      shiftL' a b = shiftL a (fromIntegral (toInteger b))

instance Tokenize LShift where
  tokenize LShift{..} = within' (Scope.InfixL LeftShift 4) $ lhs *> yield Token.Sym <* rhs

data RShift a = RShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 RShift where liftEq = genericLiftEq
instance Ord1 RShift where liftCompare = genericLiftCompare
instance Show1 RShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RShift where
  eval eval _ (RShift a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 shiftR' a' b'
    where
      shiftR' a b = shiftR a (fromIntegral (toInteger b))

instance Tokenize RShift where
  tokenize RShift{..} = within' (Scope.InfixL RightShift 4) $ lhs *> yield Token.Sym <* rhs

data UnsignedRShift a = UnsignedRShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 UnsignedRShift where liftEq = genericLiftEq
instance Ord1 UnsignedRShift where liftCompare = genericLiftCompare
instance Show1 UnsignedRShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable UnsignedRShift where
  eval eval _ (UnsignedRShift a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    unsignedRShift a' b'

newtype Complement a = Complement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Complement where liftEq = genericLiftEq
instance Ord1 Complement where liftCompare = genericLiftCompare
instance Show1 Complement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Complement where
  eval eval _ (Complement a) = do
    a' <- eval a >>= castToInteger
    liftBitwise complement a'

instance Tokenize Complement where
  tokenize Complement{..} = within' (Scope.Prefix BinaryComplement) $ yield Token.Sym <* value

-- | Member Access (e.g. a.b)
data MemberAccess a = MemberAccess { lhs :: a, rhs :: Name }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Declarations1 MemberAccess where
  liftDeclaredName _ MemberAccess{..} = Just rhs

instance Eq1 MemberAccess where liftEq = genericLiftEq
instance Ord1 MemberAccess where liftCompare = genericLiftCompare
instance Show1 MemberAccess where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable MemberAccess where
  eval eval _ MemberAccess{..} = do
    lhsValue <- eval lhs
    lhsFrame <- Abstract.scopedEnvironment lhsValue
    slot <- case lhsFrame of
      Just lhsFrame ->
        withScopeAndFrame lhsFrame $ do
          reference (Reference rhs) (Declaration rhs)
          lookupDeclaration (Declaration rhs)
      -- Throw a ReferenceError since we're attempting to reference a name within a value that is not an Object.
      Nothing -> throwEvalError (ReferenceError lhsValue rhs)
    value <- deref slot
    bindThis lhsValue value

  ref eval _ MemberAccess{..} = do
    lhsValue <- eval lhs
    lhsFrame <- Abstract.scopedEnvironment lhsValue
    case lhsFrame of
      Just lhsFrame ->
        withScopeAndFrame lhsFrame $ do
          reference (Reference rhs) (Declaration rhs)
          lookupDeclaration (Declaration rhs)
      -- Throw a ReferenceError since we're attempting to reference a name within a value that is not an Object.
      Nothing -> throwEvalError (ReferenceError lhsValue rhs)


instance Tokenize MemberAccess where
  tokenize MemberAccess{..} = lhs *> yield Access *> yield (Run (formatName rhs))

-- | Subscript (e.g a[1])
data Subscript a = Subscript { lhs :: a, rhs :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Subscript where liftEq = genericLiftEq
instance Ord1 Subscript where liftCompare = genericLiftCompare
instance Show1 Subscript where liftShowsPrec = genericLiftShowsPrec

-- TODO: Finish Eval instance for Subscript
-- TODO return a special LvalSubscript instance here
instance Evaluatable Subscript where
  eval eval _ (Subscript l [r]) = join (index <$> eval l <*> eval r)
  eval _    _ (Subscript _ _)   = throwUnspecializedError (UnspecializedError "Eval unspecialized for subscript with slices")

instance Tokenize Subscript where
  tokenize Subscript{..} = lhs *> within' Scope.Indexing (sequenceA_ (intersperse (yield Token.Sep) rhs))

data Member a = Member { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Member where liftEq = genericLiftEq
instance Ord1 Member where liftCompare = genericLiftCompare
instance Show1 Member where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Member where

instance Tokenize Member where
  tokenize Member{..} = lhs *> yield Token.Access <* rhs

-- | Enumeration (e.g. a[1:10:1] in Python (start at index 1, stop at index 10, step 1 element from start to stop))
data Enumeration a = Enumeration { enumerationStart :: !a, enumerationEnd :: !a, enumerationStep :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Enumeration where liftEq = genericLiftEq
instance Ord1 Enumeration where liftCompare = genericLiftCompare
instance Show1 Enumeration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Enumeration
instance Evaluatable Enumeration

instance Tokenize Enumeration where
  tokenize Enumeration{..} = within Scope.Slice $ enumerationStart *> enumerationEnd *> enumerationStep

-- | InstanceOf (e.g. a instanceof b in JavaScript
data InstanceOf a = InstanceOf { instanceOfSubject :: !a, instanceOfObject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InstanceOf where liftEq = genericLiftEq
instance Ord1 InstanceOf where liftCompare = genericLiftCompare
instance Show1 InstanceOf where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InstanceOf
instance Evaluatable InstanceOf


-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
newtype ScopeResolution a = ScopeResolution { scopes :: NonEmpty a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Hashable1 ScopeResolution where liftHashWithSalt = foldl
instance Eq1 ScopeResolution where liftEq = genericLiftEq
instance Ord1 ScopeResolution where liftCompare = genericLiftCompare
instance Show1 ScopeResolution where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ScopeResolution

instance Tokenize ScopeResolution where
  tokenize (ScopeResolution (a :| rest)) =
    a *> for_ rest (yield Token.Resolve *>)

instance Declarations1 ScopeResolution where
  liftDeclaredName declaredName = declaredName . NonEmpty.last . scopes

-- | A non-null expression such as Typescript or Swift's ! expression.
newtype NonNullExpression a = NonNullExpression { nonNullExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Ord1 NonNullExpression where liftCompare = genericLiftCompare
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for NonNullExpression
instance Evaluatable NonNullExpression


-- | An await expression in Javascript or C#.
newtype Await a = Await { awaitSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Await where liftEq = genericLiftEq
instance Ord1 Await where liftCompare = genericLiftCompare
instance Show1 Await where liftShowsPrec = genericLiftShowsPrec

-- TODO: Improve this to model asynchrony or capture some data suggesting async calls are not a problem.
--       We are currently dealing with an asynchronous construct synchronously.
instance Evaluatable Await where
  eval eval _ (Await a) = eval a

-- | An object constructor call in Javascript, Java, etc.
data New a = New { subject :: a , typeParameters :: a, arguments :: [a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Declarations1 New where
  liftDeclaredName declaredName New{..} = declaredName subject

instance Eq1 New where liftEq = genericLiftEq
instance Ord1 New where liftCompare = genericLiftCompare
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for New
instance Evaluatable New where
  eval eval _ New{..} = do
    name <- maybeM (throwNoNameError subject) (declaredName subject)
    assocScope <- maybeM (throwEvalError $ ConstructorError name) =<< associatedScope (Declaration name)
    objectScope <- newScope (Map.singleton Superclass [ assocScope ])
    slot <- lookupDeclaration (Declaration name)
    classVal <- deref slot
    classFrame <- maybeM (throwEvalError $ ScopedEnvError classVal) =<< scopedEnvironment classVal

    objectFrame <- newFrame objectScope (Map.singleton Superclass $ Map.singleton assocScope classFrame)
    objectVal <- object objectFrame

    classScope <- scopeLookup classFrame
    instanceMembers <- relationsOfScope classScope Instance

    void . withScopeAndFrame objectFrame $ do
      for_ instanceMembers $ \Info{..} -> do
        declare dataDeclaration Default dataSpan dataAssociatedScope

      -- TODO: This is a typescript specific name and we should allow languages to customize it.
      let constructorName = Name.name "constructor"
      reference (Reference constructorName) (Declaration constructorName)
      constructor <- deref =<< lookupDeclaration (Declaration constructorName)
      args <- traverse eval arguments
      boundConstructor <- bindThis objectVal constructor
      call boundConstructor args

    pure objectVal

-- | A cast expression to a specified type.
data Cast a =  Cast { castSubject :: !a, castType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Cast where liftEq = genericLiftEq
instance Ord1 Cast where liftCompare = genericLiftCompare
instance Show1 Cast where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Cast

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, Named1, Message1, NFData1)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Super

instance Tokenize Super where
  tokenize _ = yield Token.Superclass

data This a = This
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, Named1, Message1, NFData1)

instance Tokenize This where
  tokenize _ = yield Self

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable This where
  eval _ _ This = do
    reference (Reference __self) (Declaration __self)
    deref =<< lookupDeclaration (Declaration __self)
