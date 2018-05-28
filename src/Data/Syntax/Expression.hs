{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
module Data.Syntax.Expression where

import Data.Abstract.Evaluatable
import Data.Abstract.Number (liftIntegralFrac, liftReal, liftedExponent, liftedFloorDiv)
import Data.Fixed
import Data.JSON.Fields
import Diffing.Algorithm
import Prologue hiding (index)

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callContext :: ![a], callFunction :: !a, callParams :: ![a], callBlock :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Call where liftEq = genericLiftEq
instance Ord1 Call where liftCompare = genericLiftCompare
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Call

instance Evaluatable Call where
  eval Call{..} = do
    op <- subtermValue callFunction
    Rval <$> call op (map subtermValue callParams)

data Comparison a
  = LessThan !a !a
  | LessThanEqual !a !a
  | GreaterThan !a !a
  | GreaterThanEqual !a !a
  | Equal !a !a
  | StrictEqual !a !a
  | Comparison !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Comparison where liftEq = genericLiftEq
instance Ord1 Comparison where liftCompare = genericLiftCompare
instance Show1 Comparison where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Comparison

instance Evaluatable Comparison where
  eval t = Rval <$> (traverse subtermValue t >>= go) where
    go x = case x of
      (LessThan a b)         -> liftComparison (Concrete (<)) a b
      (LessThanEqual a b)    -> liftComparison (Concrete (<=)) a b
      (GreaterThan a b)      -> liftComparison (Concrete (>)) a b
      (GreaterThanEqual a b) -> liftComparison (Concrete (>=)) a b
      -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
      -- We need some mechanism to customize this behavior per-language.
      (Equal a b)            -> liftComparison (Concrete (==)) a b
      (StrictEqual a b)      -> liftComparison (Concrete (==)) a b
      (Comparison a b)       -> liftComparison Generalized a b

-- | Binary arithmetic operators.
data Arithmetic a
  = Plus !a !a
  | Minus !a !a
  | Times !a !a
  | DividedBy !a !a
  | FloorDivision !a !a
  | Modulo !a !a
  | Power !a !a
  | Negate !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Arithmetic where liftEq = genericLiftEq
instance Ord1 Arithmetic where liftCompare = genericLiftCompare
instance Show1 Arithmetic where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Arithmetic

instance Evaluatable Arithmetic where
  eval t = Rval <$> (traverse subtermValue t >>= go) where
    go (Plus a b)          = liftNumeric2 add a b  where add    = liftReal (+)
    go (Minus a b)         = liftNumeric2 sub a b  where sub    = liftReal (-)
    go (Times a b)         = liftNumeric2 mul a b  where mul    = liftReal (*)
    go (DividedBy a b)     = liftNumeric2 div' a b where div'   = liftIntegralFrac div (/)
    go (Modulo a b)        = liftNumeric2 mod'' a b where mod'' = liftIntegralFrac mod mod'
    go (Power a b)         = liftNumeric2 liftedExponent a b
    go (Negate a)          = liftNumeric negate a
    go (FloorDivision a b) = liftNumeric2 liftedFloorDiv a b

-- | Regex matching operators (Ruby's =~ and ~!)
data Match a
  = Matches !a !a
  | NotMatches !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Match where liftEq = genericLiftEq
instance Ord1 Match where liftCompare = genericLiftCompare
instance Show1 Match where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Match

-- TODO: Implement Eval instance for Match
instance Evaluatable Match

-- | Boolean operators.
data Boolean a
  = Or !a !a
  | And !a !a
  | Not !a
  | XOr !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Boolean

instance Evaluatable Boolean where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval t = Rval <$> go (fmap subtermValue t) where
    go (And a b) = do
      cond <- a
      ifthenelse cond b (pure cond)
    go (Or a b) = do
      cond <- a
      ifthenelse cond (pure cond) b
    go (Not a) = a >>= fmap (boolean . not) . asBool
    go (XOr a b) = boolean <$> (liftA2 (/=) (a >>= asBool) (b >>= asBool))

-- | Javascript delete operator
newtype Delete a = Delete a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Delete where liftEq = genericLiftEq
instance Ord1 Delete where liftCompare = genericLiftCompare
instance Show1 Delete where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Delete

-- TODO: Implement Eval instance for Delete
instance Evaluatable Delete


-- | A sequence expression such as Javascript or C's comma operator.
data SequenceExpression a = SequenceExpression { _firstExpression :: !a, _secondExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 SequenceExpression where liftEq = genericLiftEq
instance Ord1 SequenceExpression where liftCompare = genericLiftCompare
instance Show1 SequenceExpression where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 SequenceExpression

-- TODO: Implement Eval instance for SequenceExpression
instance Evaluatable SequenceExpression


-- | Javascript void operator
newtype Void a = Void a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Void where liftEq = genericLiftEq
instance Ord1 Void where liftCompare = genericLiftCompare
instance Show1 Void where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Void

-- TODO: Implement Eval instance for Void
instance Evaluatable Void


-- | Javascript typeof operator
newtype Typeof a = Typeof a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Typeof where liftEq = genericLiftEq
instance Ord1 Typeof where liftCompare = genericLiftCompare
instance Show1 Typeof where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Typeof

-- TODO: Implement Eval instance for Typeof
instance Evaluatable Typeof


-- | Bitwise operators.
data Bitwise a
  = BOr !a !a
  | BAnd !a !a
  | BXOr !a !a
  | LShift !a !a
  | RShift !a !a
  | UnsignedRShift !a !a
  | Complement a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Bitwise where liftEq = genericLiftEq
instance Ord1 Bitwise where liftCompare = genericLiftCompare
instance Show1 Bitwise where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Bitwise

instance Evaluatable Bitwise where
  eval t = Rval <$> (traverse subtermValue t >>= go) where
    genLShift x y = shiftL x (fromIntegral y)
    genRShift x y = shiftR x (fromIntegral y)
    go x = case x of
      (BOr a b)            -> liftBitwise2 (.|.) a b
      (BAnd a b)           -> liftBitwise2 (.&.) a b
      (BXOr a b)           -> liftBitwise2 xor a b
      (LShift a b)         -> liftBitwise2 genLShift a b
      (RShift a b)         -> liftBitwise2 genRShift a b
      (UnsignedRShift a b) -> liftBitwise2 genRShift a b
      (Complement a)       -> liftBitwise complement a

-- | Member Access (e.g. a.b)
data MemberAccess a
  = MemberAccess !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 MemberAccess where liftEq = genericLiftEq
instance Ord1 MemberAccess where liftCompare = genericLiftCompare
instance Show1 MemberAccess where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 MemberAccess

instance Evaluatable MemberAccess where
  eval (MemberAccess obj prop) = do
    obj <- subtermValue obj
    prop <- subtermRef prop
    case prop of
      LvalLocal propName -> pure (LvalMember obj propName)
      _ -> raiseEff (Prologue.fail "Non-Identifier as right hand side of MemberAccess!")

-- | Subscript (e.g a[1])
data Subscript a
  = Subscript !a ![a]
  | Member !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Subscript where liftEq = genericLiftEq
instance Ord1 Subscript where liftCompare = genericLiftCompare
instance Show1 Subscript where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Subscript

-- TODO: Finish Eval instance for Subscript
-- TODO return a special LvalSubscript instance here
instance Evaluatable Subscript where
  eval (Subscript l [r]) = Rval <$> join (index <$> subtermValue l <*> subtermValue r)
  eval (Subscript _ _)   = throwResumable (Unspecialized "Eval unspecialized for subscript with slices")
  eval (Member _ _)      = throwResumable (Unspecialized "Eval unspecialized for member access")


-- | Enumeration (e.g. a[1:10:1] in Python (start at index 1, stop at index 10, step 1 element from start to stop))
data Enumeration a = Enumeration { enumerationStart :: !a, enumerationEnd :: !a, enumerationStep :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Enumeration where liftEq = genericLiftEq
instance Ord1 Enumeration where liftCompare = genericLiftCompare
instance Show1 Enumeration where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Enumeration

-- TODO: Implement Eval instance for Enumeration
instance Evaluatable Enumeration


-- | InstanceOf (e.g. a instanceof b in JavaScript
data InstanceOf a = InstanceOf { instanceOfSubject :: !a, instanceOfObject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 InstanceOf where liftEq = genericLiftEq
instance Ord1 InstanceOf where liftCompare = genericLiftCompare
instance Show1 InstanceOf where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 InstanceOf

-- TODO: Implement Eval instance for InstanceOf
instance Evaluatable InstanceOf


-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
newtype ScopeResolution a = ScopeResolution [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 ScopeResolution where liftEq = genericLiftEq
instance Ord1 ScopeResolution where liftCompare = genericLiftCompare
instance Show1 ScopeResolution where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 ScopeResolution

-- TODO: Implement Eval instance for ScopeResolution
instance Evaluatable ScopeResolution


-- | A non-null expression such as Typescript or Swift's ! expression.
newtype NonNullExpression a = NonNullExpression { nonNullExpression :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Ord1 NonNullExpression where liftCompare = genericLiftCompare
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 NonNullExpression

-- TODO: Implement Eval instance for NonNullExpression
instance Evaluatable NonNullExpression


-- | An await expression in Javascript or C#.
newtype Await a = Await { awaitSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Await where liftEq = genericLiftEq
instance Ord1 Await where liftCompare = genericLiftCompare
instance Show1 Await where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Await

-- TODO: Implement Eval instance for Await
instance Evaluatable Await


-- | An object constructor call in Javascript, Java, etc.
newtype New a = New { newSubject :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 New where liftEq = genericLiftEq
instance Ord1 New where liftCompare = genericLiftCompare
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 New

-- TODO: Implement Eval instance for New
instance Evaluatable New


-- | A cast expression to a specified type.
data Cast a =  Cast { castSubject :: !a, castType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Cast where liftEq = genericLiftEq
instance Ord1 Cast where liftCompare = genericLiftCompare
instance Show1 Cast where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Cast

-- TODO: Implement Eval instance for Cast
instance Evaluatable Cast
