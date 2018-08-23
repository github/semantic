{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Expression where

import Data.Abstract.Evaluatable hiding (Member)
import Data.Abstract.Number (liftIntegralFrac, liftReal, liftedExponent, liftedFloorDiv)
import Data.Fixed
import Data.JSON.Fields
import Diffing.Algorithm hiding (Delete)
import Prologue hiding (index, Member, This, null)
import Prelude hiding (null)
import Proto3.Suite.Class

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callContext :: ![a], callFunction :: !a, callParams :: ![a], callBlock :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Call where liftEq = genericLiftEq
instance Ord1 Call where liftCompare = genericLiftCompare
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Call where
  eval Call{..} = do
    op <- subtermValue callFunction
    recv <- box unit -- TODO
    args <- traverse subtermAddress callParams
    Rval <$> call op recv args

data LessThan a = LessThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 LessThan where liftEq = genericLiftEq
instance Ord1 LessThan where liftCompare = genericLiftCompare
instance Show1 LessThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThan where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      (LessThan a b)         -> liftComparison (Concrete (<)) a b

data LessThanEqual a = LessThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 LessThanEqual where liftEq = genericLiftEq
instance Ord1 LessThanEqual where liftCompare = genericLiftCompare
instance Show1 LessThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThanEqual where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      (LessThanEqual a b)         -> liftComparison (Concrete (<=)) a b

data GreaterThan a = GreaterThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 GreaterThan where liftEq = genericLiftEq
instance Ord1 GreaterThan where liftCompare = genericLiftCompare
instance Show1 GreaterThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThan where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      (GreaterThan a b)         -> liftComparison (Concrete (>)) a b

data GreaterThanEqual a = GreaterThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 GreaterThanEqual where liftEq = genericLiftEq
instance Ord1 GreaterThanEqual where liftCompare = genericLiftCompare
instance Show1 GreaterThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThanEqual where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      (GreaterThanEqual a b)         -> liftComparison (Concrete (>=)) a b

data Equal a = Equal { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Equal where liftEq = genericLiftEq
instance Ord1 Equal where liftCompare = genericLiftCompare
instance Show1 Equal where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Equal where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
      -- We need some mechanism to customize this behavior per-language.
      (Equal a b)         -> liftComparison (Concrete (==)) a b

data StrictEqual a = StrictEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 StrictEqual where liftEq = genericLiftEq
instance Ord1 StrictEqual where liftCompare = genericLiftCompare
instance Show1 StrictEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictEqual where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
      -- We need some mechanism to customize this behavior per-language.
      (StrictEqual a b)         -> liftComparison (Concrete (==)) a b

data Comparison a = Comparison { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Comparison where liftEq = genericLiftEq
instance Ord1 Comparison where liftCompare = genericLiftCompare
instance Show1 Comparison where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comparison where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go x = case x of
      (Comparison a b)         -> liftComparison (Concrete (==)) a b

data Plus a = Plus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Plus where liftEq = genericLiftEq
instance Ord1 Plus where liftCompare = genericLiftCompare
instance Show1 Plus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Plus where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Plus a b)          = liftNumeric2 add a b  where add    = liftReal (+)

data Minus a = Minus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Minus where liftEq = genericLiftEq
instance Ord1 Minus where liftCompare = genericLiftCompare
instance Show1 Minus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Minus where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Minus a b)         = liftNumeric2 sub a b  where sub    = liftReal (-)

data Times a = Times { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Times where liftEq = genericLiftEq
instance Ord1 Times where liftCompare = genericLiftCompare
instance Show1 Times where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Times where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Times a b)         = liftNumeric2 mul a b  where mul    = liftReal (*)

data DividedBy a = DividedBy { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 DividedBy where liftEq = genericLiftEq
instance Ord1 DividedBy where liftCompare = genericLiftCompare
instance Show1 DividedBy where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DividedBy where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (DividedBy a b)     = liftNumeric2 div' a b where div'   = liftIntegralFrac div (/)

data Modulo a = Modulo { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Modulo where liftEq = genericLiftEq
instance Ord1 Modulo where liftCompare = genericLiftCompare
instance Show1 Modulo where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Modulo where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Modulo a b)        = liftNumeric2 mod'' a b where mod'' = liftIntegralFrac mod mod'

data Power a = Power { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Power where liftEq = genericLiftEq
instance Ord1 Power where liftCompare = genericLiftCompare
instance Show1 Power where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Power where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Power a b)         = liftNumeric2 liftedExponent a b

newtype Negate a = Negate { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Negate where liftEq = genericLiftEq
instance Ord1 Negate where liftCompare = genericLiftCompare
instance Show1 Negate where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Negate where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (Negate a)          = liftNumeric negate a

data FloorDivision a = FloorDivision { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 FloorDivision where liftEq = genericLiftEq
instance Ord1 FloorDivision where liftCompare = genericLiftCompare
instance Show1 FloorDivision where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FloorDivision where
  eval t = rvalBox =<< (traverse subtermValue t >>= go) where
    go (FloorDivision a b) = liftNumeric2 liftedFloorDiv a b

-- | Regex matching operators (Ruby's =~ and ~!)
data Matches a = Matches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Matches where liftEq = genericLiftEq
instance Ord1 Matches where liftCompare = genericLiftCompare
instance Show1 Matches where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Matches

data NotMatches a = NotMatches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 NotMatches where liftEq = genericLiftEq
instance Ord1 NotMatches where liftCompare = genericLiftCompare
instance Show1 NotMatches where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NotMatches

data Or a = Or { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Or where liftEq = genericLiftEq
instance Ord1 Or where liftCompare = genericLiftCompare
instance Show1 Or where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Or where
  eval (Or a b) = disjunction (subtermValue a) (subtermValue b) >>= rvalBox

data And a = And { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 And where liftEq = genericLiftEq
instance Ord1 And where liftCompare = genericLiftCompare
instance Show1 And where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable And where
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (And a b) = do
      cond <- a
      ifthenelse cond b (pure cond)

newtype Not a = Not { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Not where liftEq = genericLiftEq
instance Ord1 Not where liftCompare = genericLiftCompare
instance Show1 Not where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Not where
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (Not a) = a >>= asBool >>= boolean . not

data XOr a = XOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 XOr where liftEq = genericLiftEq
instance Ord1 XOr where liftCompare = genericLiftCompare
instance Show1 XOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable XOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (XOr a b) = liftA2 (/=) (a >>= asBool) (b >>= asBool) >>= boolean

-- | Javascript delete operator
newtype Delete a = Delete { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Delete where liftEq = genericLiftEq
instance Ord1 Delete where liftCompare = genericLiftCompare
instance Show1 Delete where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Delete
instance Evaluatable Delete where
  eval (Delete a) = do
    valueRef <- subtermRef a
    addr <- address valueRef
    dealloc addr
    rvalBox unit

-- | A sequence expression such as Javascript or C's comma operator.
data SequenceExpression a = SequenceExpression { firstExpression :: !a, secondExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 SequenceExpression where liftEq = genericLiftEq
instance Ord1 SequenceExpression where liftCompare = genericLiftCompare
instance Show1 SequenceExpression where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for SequenceExpression
instance Evaluatable SequenceExpression where
  eval (SequenceExpression a b) =
    subtermValue a >> subtermRef b

-- | Javascript void operator
newtype Void a = Void { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Void where liftEq = genericLiftEq
instance Ord1 Void where liftCompare = genericLiftCompare
instance Show1 Void where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Void
instance Evaluatable Void where
  eval (Void a) =
    subtermValue a >> rvalBox null

-- | Javascript typeof operator
newtype Typeof a = Typeof { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Typeof where liftEq = genericLiftEq
instance Ord1 Typeof where liftCompare = genericLiftCompare
instance Show1 Typeof where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Typeof
instance Evaluatable Typeof

-- | Bitwise operators.
data BOr a = BOr { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 BOr where liftEq = genericLiftEq
instance Ord1 BOr where liftCompare = genericLiftCompare
instance Show1 BOr where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BOr where

data BAnd a = BAnd { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 BAnd where liftEq = genericLiftEq
instance Ord1 BAnd where liftCompare = genericLiftCompare
instance Show1 BAnd where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BAnd where

data BXOr a = BXOr { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 BXOr where liftEq = genericLiftEq
instance Ord1 BXOr where liftCompare = genericLiftCompare
instance Show1 BXOr where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BXOr where

data LShift a = LShift { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 LShift where liftEq = genericLiftEq
instance Ord1 LShift where liftCompare = genericLiftCompare
instance Show1 LShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LShift where

data RShift a = RShift { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 RShift where liftEq = genericLiftEq
instance Ord1 RShift where liftCompare = genericLiftCompare
instance Show1 RShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RShift where

data UnsignedRShift a = UnsignedRShift { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 UnsignedRShift where liftEq = genericLiftEq
instance Ord1 UnsignedRShift where liftCompare = genericLiftCompare
instance Show1 UnsignedRShift where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable UnsignedRShift where

newtype Complement a = Complement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Complement where liftEq = genericLiftEq
instance Ord1 Complement where liftCompare = genericLiftCompare
instance Show1 Complement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Complement where

-- | Member Access (e.g. a.b)
data MemberAccess a = MemberAccess { lhs :: a, rhs :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 MemberAccess where liftEq = genericLiftEq
instance Ord1 MemberAccess where liftCompare = genericLiftCompare
instance Show1 MemberAccess where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable MemberAccess where
  eval (MemberAccess obj propName) = do
    ptr <- subtermAddress obj
    pure $! LvalMember ptr propName

-- | Subscript (e.g a[1])
data Subscript a = Subscript { lhs :: a, rhs :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Subscript where liftEq = genericLiftEq
instance Ord1 Subscript where liftCompare = genericLiftCompare
instance Show1 Subscript where liftShowsPrec = genericLiftShowsPrec

-- TODO: Finish Eval instance for Subscript
-- TODO return a special LvalSubscript instance here
instance Evaluatable Subscript where
  eval (Subscript l [r]) = Rval <$> join (index <$> subtermValue l <*> subtermValue r)
  eval (Subscript _ _)   = rvalBox =<< throwUnspecializedError (UnspecializedError "Eval unspecialized for subscript with slices")

data Member a = Member { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Member where liftEq = genericLiftEq
instance Ord1 Member where liftCompare = genericLiftCompare
instance Show1 Member where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Member where

-- | Enumeration (e.g. a[1:10:1] in Python (start at index 1, stop at index 10, step 1 element from start to stop))
data Enumeration a = Enumeration { enumerationStart :: !a, enumerationEnd :: !a, enumerationStep :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Enumeration where liftEq = genericLiftEq
instance Ord1 Enumeration where liftCompare = genericLiftCompare
instance Show1 Enumeration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Enumeration
instance Evaluatable Enumeration


-- | InstanceOf (e.g. a instanceof b in JavaScript
data InstanceOf a = InstanceOf { instanceOfSubject :: !a, instanceOfObject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 InstanceOf where liftEq = genericLiftEq
instance Ord1 InstanceOf where liftCompare = genericLiftCompare
instance Show1 InstanceOf where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InstanceOf
instance Evaluatable InstanceOf


-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
newtype ScopeResolution a = ScopeResolution { scopes :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Hashable1 ScopeResolution where liftHashWithSalt = foldl
instance Eq1 ScopeResolution where liftEq = genericLiftEq
instance Ord1 ScopeResolution where liftCompare = genericLiftCompare
instance Show1 ScopeResolution where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ScopeResolution where
  eval (ScopeResolution xs) = Rval <$> foldl1 f (fmap subtermAddress xs)
    where f ns id = ns >>= flip evaluateInScopedEnv id


-- | A non-null expression such as Typescript or Swift's ! expression.
newtype NonNullExpression a = NonNullExpression { nonNullExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Ord1 NonNullExpression where liftCompare = genericLiftCompare
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for NonNullExpression
instance Evaluatable NonNullExpression


-- | An await expression in Javascript or C#.
newtype Await a = Await { awaitSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Await where liftEq = genericLiftEq
instance Ord1 Await where liftCompare = genericLiftCompare
instance Show1 Await where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Await
instance Evaluatable Await


-- | An object constructor call in Javascript, Java, etc.
newtype New a = New { newSubject :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 New where liftEq = genericLiftEq
instance Ord1 New where liftCompare = genericLiftCompare
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for New
instance Evaluatable New

-- | A cast expression to a specified type.
data Cast a =  Cast { castSubject :: !a, castType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Cast where liftEq = genericLiftEq
instance Ord1 Cast where liftCompare = genericLiftCompare
instance Show1 Cast where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Cast
instance Evaluatable Cast

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Super

data This a = This
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, Named1, Message1)

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable This where
  eval This = Rval <$> (maybeM (box unit) =<< self)
