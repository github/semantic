{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}
module Data.Syntax.Expression (module Data.Syntax.Expression) where

import Prelude hiding (null)
import Prologue hiding (index, null)

import           Control.Abstract hiding (Bitwise (..), Call, Member)
import           Data.Abstract.Evaluatable as Abstract hiding (Member)
import           Data.Abstract.Name as Name
import           Data.Abstract.Number (liftIntegralFrac, liftReal, liftedExponent, liftedFloorDiv)
import           Data.Fixed
import           Data.JSON.Fields
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Diffing.Algorithm hiding (Delete)
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callContext :: ![a], callFunction :: !a, callParams :: ![a], callBlock :: !a }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Call where liftEq = genericLiftEq
instance Ord1 Call where liftCompare = genericLiftCompare
instance Show1 Call where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 Call where
  liftDeclaredName declaredName Call{..} = declaredName callFunction

instance Evaluatable Call where
  eval eval _ Call{..} = do
    op <- eval callFunction
    args <- traverse eval callParams
    call op args

data LessThan a = LessThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LessThan where liftEq = genericLiftEq
instance Ord1 LessThan where liftCompare = genericLiftCompare
instance Show1 LessThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThan where
  eval eval _ t = traverse eval t >>= go where
    go (LessThan a b) = liftComparison (Concrete (<)) a b

data LessThanEqual a = LessThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LessThanEqual where liftEq = genericLiftEq
instance Ord1 LessThanEqual where liftCompare = genericLiftCompare
instance Show1 LessThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LessThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (LessThanEqual a b) = liftComparison (Concrete (<=)) a b

data GreaterThan a = GreaterThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 GreaterThan where liftEq = genericLiftEq
instance Ord1 GreaterThan where liftCompare = genericLiftCompare
instance Show1 GreaterThan where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThan where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThan a b) = liftComparison (Concrete (>)) a b

data GreaterThanEqual a = GreaterThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 GreaterThanEqual where liftEq = genericLiftEq
instance Ord1 GreaterThanEqual where liftCompare = genericLiftCompare
instance Show1 GreaterThanEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GreaterThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThanEqual a b) = liftComparison (Concrete (>=)) a b

data Equal a = Equal { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Equal where liftEq = genericLiftEq
instance Ord1 Equal where liftCompare = genericLiftCompare
instance Show1 Equal where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Equal where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (Equal a b) = liftComparison (Concrete (==)) a b

data StrictEqual a = StrictEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 StrictEqual where liftEq = genericLiftEq
instance Ord1 StrictEqual where liftCompare = genericLiftCompare
instance Show1 StrictEqual where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictEqual where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (StrictEqual a b) = liftComparison (Concrete (==)) a b

data Comparison a = Comparison { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Comparison where liftEq = genericLiftEq
instance Ord1 Comparison where liftCompare = genericLiftCompare
instance Show1 Comparison where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comparison where
  eval eval _ t = traverse eval t >>= go where
    go (Comparison a b) = liftComparison (Concrete (==)) a b

data Plus a = Plus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Plus where liftEq = genericLiftEq
instance Ord1 Plus where liftCompare = genericLiftCompare
instance Show1 Plus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Plus where
  eval eval _ t = traverse eval t >>= go where
    go (Plus a b) = liftNumeric2 add a b  where add    = liftReal (+)

data Minus a = Minus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Minus where liftEq = genericLiftEq
instance Ord1 Minus where liftCompare = genericLiftCompare
instance Show1 Minus where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Minus where
  eval eval _ t = traverse eval t >>= go where
    go (Minus a b) = liftNumeric2 (liftReal (-)) a b

data Times a = Times { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Times where liftEq = genericLiftEq
instance Ord1 Times where liftCompare = genericLiftCompare
instance Show1 Times where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Times where
  eval eval _ t = traverse eval t >>= go where
    go (Times a b) = liftNumeric2 (liftReal (*)) a b

data DividedBy a = DividedBy { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 DividedBy where liftEq = genericLiftEq
instance Ord1 DividedBy where liftCompare = genericLiftCompare
instance Show1 DividedBy where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DividedBy where
  eval eval _ t = traverse eval t >>= go where
    go (DividedBy a b) = liftNumeric2 (liftIntegralFrac div (/)) a b

data Modulo a = Modulo { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Modulo where liftEq = genericLiftEq
instance Ord1 Modulo where liftCompare = genericLiftCompare
instance Show1 Modulo where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Modulo where
  eval eval _ t = traverse eval t >>= go where
    go (Modulo a b) = liftNumeric2 (liftIntegralFrac mod mod') a b

data Power a = Power { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Power where liftEq = genericLiftEq
instance Ord1 Power where liftCompare = genericLiftCompare
instance Show1 Power where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Power where
  eval eval _ t = traverse eval t >>= go where
    go (Power a b) = liftNumeric2 liftedExponent a b

newtype Negate a = Negate { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Negate where liftEq = genericLiftEq
instance Ord1 Negate where liftCompare = genericLiftCompare
instance Show1 Negate where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Negate where
  eval eval _ t = traverse eval t >>= go where
    go (Negate a) = liftNumeric negate a

data FloorDivision a = FloorDivision { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 FloorDivision where liftEq = genericLiftEq
instance Ord1 FloorDivision where liftCompare = genericLiftCompare
instance Show1 FloorDivision where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FloorDivision where
  eval eval _ t = traverse eval t >>= go where
    go (FloorDivision a b) = liftNumeric2 liftedFloorDiv a b

-- | Regex matching operators (Ruby's =~ and ~!)
data Matches a = Matches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Matches where liftEq = genericLiftEq
instance Ord1 Matches where liftCompare = genericLiftCompare
instance Show1 Matches where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Matches

data NotMatches a = NotMatches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NotMatches where liftEq = genericLiftEq
instance Ord1 NotMatches where liftCompare = genericLiftCompare
instance Show1 NotMatches where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NotMatches

data Or a = Or { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Or where liftEq = genericLiftEq
instance Ord1 Or where liftCompare = genericLiftCompare
instance Show1 Or where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Or where
  eval eval _ (Or a b) = do
    a' <- eval a
    ifthenelse a' (pure a') (eval b)

data And a = And { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 And where liftEq = genericLiftEq
instance Ord1 And where liftCompare = genericLiftCompare
instance Show1 And where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable And where
  eval eval _ (And a b) = do
    a' <- eval a
    ifthenelse a' (eval b) (pure a')

newtype Not a = Not { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Not where liftEq = genericLiftEq
instance Ord1 Not where liftCompare = genericLiftCompare
instance Show1 Not where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Not where
  eval eval _ (Not a) = eval a >>= asBool >>= boolean . not

data XOr a = XOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 XOr where liftEq = genericLiftEq
instance Ord1 XOr where liftCompare = genericLiftCompare
instance Show1 XOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable XOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ (XOr a b) = liftA2 (/=) (eval a >>= asBool) (eval b >>= asBool) >>= boolean

-- | Javascript delete operator
newtype Delete a = Delete { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Delete where liftEq = genericLiftEq
instance Ord1 Delete where liftCompare = genericLiftCompare
instance Show1 Delete where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Delete where
  eval _ ref (Delete a) = ref a >>= dealloc >> unit

-- | A sequence expression such as Javascript or C's comma operator.
data SequenceExpression a = SequenceExpression { firstExpression :: !a, secondExpression :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 SequenceExpression where liftEq = genericLiftEq
instance Ord1 SequenceExpression where liftCompare = genericLiftCompare
instance Show1 SequenceExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SequenceExpression where
  eval eval _ (SequenceExpression a b) =
    eval a >> eval b

-- | Javascript void operator
newtype Void a = Void { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Void where liftEq = genericLiftEq
instance Ord1 Void where liftCompare = genericLiftCompare
instance Show1 Void where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Void where
  eval eval _ (Void a) =
    eval a >> pure null

-- | Javascript typeof operator
newtype Typeof a = Typeof { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Typeof where liftEq = genericLiftEq
instance Ord1 Typeof where liftCompare = genericLiftCompare
instance Show1 Typeof where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Typeof
instance Evaluatable Typeof

-- | Bitwise operators.
data BOr a = BOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 BOr where liftEq = genericLiftEq
instance Ord1 BOr where liftCompare = genericLiftCompare
instance Show1 BOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable BOr where
  eval eval _ (BOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.|.) a' b'

data BAnd a = BAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 BAnd where liftEq = genericLiftEq
instance Ord1 BAnd where liftCompare = genericLiftCompare
instance Show1 BAnd where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable BAnd where
  eval eval _ (BAnd a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.&.) a' b'

data BXOr a = BXOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 BXOr where liftEq = genericLiftEq
instance Ord1 BXOr where liftCompare = genericLiftCompare
instance Show1 BXOr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable BXOr where
  eval eval _ (BXOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 xor a' b'

data LShift a = LShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

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

data RShift a = RShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

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

data UnsignedRShift a = UnsignedRShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 UnsignedRShift where liftEq = genericLiftEq
instance Ord1 UnsignedRShift where liftCompare = genericLiftCompare
instance Show1 UnsignedRShift where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable UnsignedRShift where
  eval eval _ (UnsignedRShift a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    unsignedRShift a' b'

newtype Complement a = Complement { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Complement where liftEq = genericLiftEq
instance Ord1 Complement where liftCompare = genericLiftCompare
instance Show1 Complement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Complement where
  eval eval _ (Complement a) = do
    a' <- eval a >>= castToInteger
    liftBitwise complement a'

-- | Member Access (e.g. a.b)
data MemberAccess a = MemberAccess { lhs :: a, rhs :: a }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 MemberAccess where liftEq = genericLiftEq
instance Ord1 MemberAccess where liftCompare = genericLiftCompare
instance Show1 MemberAccess where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 MemberAccess where
  liftDeclaredName declaredName MemberAccess{..} = declaredName rhs

instance Evaluatable MemberAccess where
  eval eval ref MemberAccess{..} = do
    lhsValue <- eval lhs
    lhsFrame <- Abstract.scopedEnvironment lhsValue

    rhsSlot <- case lhsFrame of
      Just lhsFrame ->
        -- FIXME: The span is not set up correctly when calling `ref` so we have to eval
        -- it first
        withScopeAndFrame lhsFrame (eval rhs >> ref rhs)
      -- Throw a ReferenceError since we're attempting to reference a name within a value that is not an Object.
      Nothing -> throwEvalError (ReferenceError lhsValue rhs)

    rhsValue <- deref rhsSlot
    rhsScope <- scopeLookup (frameAddress rhsSlot)

    let lhsAccessControl = fromMaybe Public (termToAccessControl lhs)
    infos <- declarationsByAccessControl rhsScope lhsAccessControl

    -- This means we always throw an 'AccessControlError' whenever we have a rhs term whose 'declaredName' is 'Nothing'.
    rhsName <- maybeM (throwNoNameError rhs) (declaredName rhs)
    rhsValue' <- case find (\Info{..} -> Declaration rhsName == infoDeclaration) infos of
      Just _  -> pure rhsValue
      Nothing -> do
        let lhsName = fromMaybe (name "") (declaredName lhs)
        info <- declarationByName rhsScope (Declaration rhsName)
        throwEvalError $ AccessControlError (lhsName, lhsAccessControl) (rhsName, infoAccessControl info) rhsValue

    bindThis lhsValue rhsValue'


  ref eval ref' MemberAccess{..} = do
    lhsValue <- eval lhs
    lhsFrame <- Abstract.scopedEnvironment lhsValue
    case lhsFrame of
      Just lhsFrame -> withScopeAndFrame lhsFrame (ref' rhs)
      -- Throw a ReferenceError since we're attempting to reference a name within a value that is not an Object.
      Nothing -> throwEvalError (ReferenceError lhsValue rhs)


-- | Subscript (e.g a[1])
data Subscript a = Subscript { lhs :: a, rhs :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Subscript where liftEq = genericLiftEq
instance Ord1 Subscript where liftCompare = genericLiftCompare
instance Show1 Subscript where liftShowsPrec = genericLiftShowsPrec

-- TODO: Finish Eval instance for Subscript
-- TODO return a special LvalSubscript instance here
instance Evaluatable Subscript where
  eval eval _ (Subscript l [r]) = join (index <$> eval l <*> eval r)
  eval _    _ (Subscript _ _)   = throwUnspecializedError (UnspecializedError "Eval unspecialized for subscript with slices")

data Member a = Member { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Member where liftEq = genericLiftEq
instance Ord1 Member where liftCompare = genericLiftCompare
instance Show1 Member where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Member where

-- | Enumeration (e.g. a[1:10:1] in Python (start at index 1, stop at index 10, step 1 element from start to stop))
data Enumeration a = Enumeration { enumerationStart :: !a, enumerationEnd :: !a, enumerationStep :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Enumeration where liftEq = genericLiftEq
instance Ord1 Enumeration where liftCompare = genericLiftCompare
instance Show1 Enumeration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Enumeration
instance Evaluatable Enumeration

-- | InstanceOf (e.g. a instanceof b in JavaScript
data InstanceOf a = InstanceOf { instanceOfSubject :: !a, instanceOfObject :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InstanceOf where liftEq = genericLiftEq
instance Ord1 InstanceOf where liftCompare = genericLiftCompare
instance Show1 InstanceOf where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InstanceOf
instance Evaluatable InstanceOf


-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
newtype ScopeResolution a = ScopeResolution { scopes :: NonEmpty a }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, ToJSONFields1, Traversable)

instance Eq1 ScopeResolution where liftEq = genericLiftEq
instance Ord1 ScopeResolution where liftCompare = genericLiftCompare
instance Show1 ScopeResolution where liftShowsPrec = genericLiftShowsPrec

instance Hashable1 ScopeResolution where liftHashWithSalt = foldl

instance Evaluatable ScopeResolution

instance Declarations1 ScopeResolution where
  liftDeclaredName declaredName = declaredName . NonEmpty.last . scopes

-- | A non-null expression such as Typescript or Swift's ! expression.
newtype NonNullExpression a = NonNullExpression { nonNullExpression :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Ord1 NonNullExpression where liftCompare = genericLiftCompare
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for NonNullExpression
instance Evaluatable NonNullExpression


-- | An await expression in Javascript or C#.
newtype Await a = Await { awaitSubject :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Await where liftEq = genericLiftEq
instance Ord1 Await where liftCompare = genericLiftCompare
instance Show1 Await where liftShowsPrec = genericLiftShowsPrec
-- TODO: Improve this to model asynchrony or capture some data suggesting async calls are not a problem.
--       We are currently dealing with an asynchronous construct synchronously.
instance Evaluatable Await where
  eval eval _ (Await a) = eval a

-- | An object constructor call in Javascript, Java, etc.
data New a = New { newSubject :: a , newTypeParameters :: a, newArguments :: [a] }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 New where liftEq = genericLiftEq
instance Ord1 New where liftCompare = genericLiftCompare
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 New where
  liftDeclaredName declaredName New{..} = declaredName newSubject

-- TODO: Implement Eval instance for New
instance Evaluatable New where
  eval eval _ New{..} = do
    name <- maybeM (throwNoNameError newSubject) (declaredName newSubject)
    assocScope <- maybeM (throwEvalError $ ConstructorError name) =<< associatedScope (Declaration name)
    objectScope <- newScope (Map.singleton Superclass [ assocScope ])
    slot <- lookupSlot (Declaration name)
    classVal <- deref slot
    classFrame <- maybeM (throwEvalError $ ScopedEnvError classVal) =<< scopedEnvironment classVal

    objectFrame <- newFrame objectScope (Map.singleton Superclass $ Map.singleton assocScope classFrame)
    objectVal <- object objectFrame

    classScope <- scopeLookup classFrame
    instanceMembers <- declarationsByRelation classScope Instance

    void . withScopeAndFrame objectFrame $ do
      for_ instanceMembers $ \Info{..} -> do
        declare infoDeclaration Default infoAccessControl infoSpan infoKind infoAssociatedScope

      -- TODO: This is a typescript specific name and we should allow languages to customize it.
      let constructorName = Name.name "constructor"
      maybeConstructor <- maybeLookupDeclaration (Declaration constructorName)
      case maybeConstructor of
        Just slot -> do
          span <- ask @Span
          reference (Reference constructorName) span ScopeGraph.New (Declaration constructorName)
          constructor <- deref slot
          args <- traverse eval newArguments
          boundConstructor <- bindThis objectVal constructor
          call boundConstructor args
        Nothing -> pure objectVal

    pure objectVal

-- | A cast expression to a specified type.
data Cast a =  Cast { castSubject :: !a, castType :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Cast where liftEq = genericLiftEq
instance Ord1 Cast where liftCompare = genericLiftCompare
instance Show1 Cast where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Cast

data Super a = Super
  deriving (Diffable, Foldable, Functor,  Generic1, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Super

data This a = This
  deriving (Diffable, Foldable, Functor,  Generic1, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1)

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable This where
  eval _ _ This = do
    span <- ask @Span
    reference (Reference __self) span ScopeGraph.This (Declaration __self)
    deref =<< lookupSlot (Declaration __self)

instance AccessControls1 This where
  liftTermToAccessControl _ _ = Just Private
