{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Expression where

import Prelude hiding (null)
import Prologue hiding (This, index, null)

import           Control.Abstract hiding (Bitwise (..), Call, Member, Void)
import           Data.Abstract.Evaluatable as Abstract hiding (Member, Void)
import           Data.Abstract.Name as Name
import           Data.Abstract.Number (liftIntegralFrac, liftReal, liftedExponent, liftedFloorDiv)
import           Data.Fixed
import           Data.JSON.Fields
import           Data.List (intersperse)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import qualified Data.Reprinting.Token as Token
import           Diffing.Algorithm hiding (Delete)
import           Reprinting.Tokenize hiding (Superclass)
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- | Typical prefix function application, like `f(x)` in many languages, or `f x` in Haskell.
data Call a = Call { callContext :: ![a], callFunction :: !a, callParams :: ![a], callBlock :: !a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Call

instance Declarations1 Call where
  liftDeclaredName declaredName Call{..} = declaredName callFunction

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
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LessThan

instance Evaluatable LessThan where
  eval eval _ t = traverse eval t >>= go where
    go (LessThan a b) = liftComparison (Concrete (<)) a b

instance Tokenize LessThan where
  tokenize LessThan{..} = within' (Scope.InfixL (Compare Less) 4) $ lhs *> yield Token.Sym <* rhs

data LessThanEqual a = LessThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LessThanEqual

instance Evaluatable LessThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (LessThanEqual a b) = liftComparison (Concrete (<=)) a b

instance Tokenize LessThanEqual where
  tokenize LessThanEqual{..} = within' (Scope.InfixL (CompareEql Less) 4) $ lhs *> yield Token.Sym <* rhs

data GreaterThan a = GreaterThan { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically GreaterThan

instance Evaluatable GreaterThan where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThan a b) = liftComparison (Concrete (>)) a b

instance Tokenize GreaterThan where
  tokenize GreaterThan{..} = within' (Scope.InfixL (Compare Greater) 4) $ lhs *> yield Token.Sym <* rhs

data GreaterThanEqual a = GreaterThanEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically GreaterThanEqual

instance Evaluatable GreaterThanEqual where
  eval eval _ t = traverse eval t >>= go where
    go (GreaterThanEqual a b) = liftComparison (Concrete (>=)) a b

instance Tokenize GreaterThanEqual where
  tokenize GreaterThanEqual{..} = within' (Scope.InfixL (CompareEql Greater) 4) $ lhs *> yield Token.Sym <* rhs

data Equal a = Equal { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Equal

instance Evaluatable Equal where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (Equal a b) = liftComparison (Concrete (==)) a b

instance Tokenize Equal where
  tokenize Equal{..} = within' (Scope.InfixL Eql 4) $ lhs *> yield Token.Sym <* rhs

data StrictEqual a = StrictEqual { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StrictEqual

instance Evaluatable StrictEqual where
  eval eval _ t = traverse eval t >>= go where
    -- TODO: in PHP and JavaScript, the equals operator performs type coercion.
    -- We need some mechanism to customize this behavior per-language.
    go (StrictEqual a b) = liftComparison (Concrete (==)) a b

instance Tokenize StrictEqual where
  tokenize StrictEqual{..} = within' (Scope.InfixL StrictEql 4) $ lhs *> yield Token.Sym <* rhs

data Comparison a = Comparison { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Comparison

instance Evaluatable Comparison where
  eval eval _ t = traverse eval t >>= go where
    go (Comparison a b) = liftComparison (Concrete (==)) a b

instance Tokenize Comparison where
  tokenize Comparison{..} = within' (Scope.InfixL Spaceship 4) $ lhs *> yield Token.Sym <* rhs

data Plus a = Plus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Plus

instance Evaluatable Plus where
  eval eval _ t = traverse eval t >>= go where
    go (Plus a b) = liftNumeric2 add a b  where add    = liftReal (+)

instance Tokenize Plus where
  tokenize Plus{..} = within' (Scope.InfixL Add 6) $ lhs *> yield Token.Sym <* rhs

data Minus a = Minus { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Minus

instance Evaluatable Minus where
  eval eval _ t = traverse eval t >>= go where
    go (Minus a b) = liftNumeric2 (liftReal (-)) a b

instance Tokenize Minus where
  tokenize Minus{..} = within' (Scope.InfixL Subtract 6) $ lhs *> yield Token.Sym <* rhs

data Times a = Times { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Times

instance Evaluatable Times where
  eval eval _ t = traverse eval t >>= go where
    go (Times a b) = liftNumeric2 (liftReal (*)) a b

instance Tokenize Times where
  tokenize Times{..} = within' (Scope.InfixL Multiply 7) $ lhs *> yield Token.Sym <* rhs

data DividedBy a = DividedBy { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DividedBy

instance Evaluatable DividedBy where
  eval eval _ t = traverse eval t >>= go where
    go (DividedBy a b) = liftNumeric2 (liftIntegralFrac div (/)) a b

instance Tokenize DividedBy where
  tokenize DividedBy{..} = within' (Scope.InfixL Divide 7) $ lhs *> yield Token.Sym <* rhs

data Modulo a = Modulo { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Modulo

instance Evaluatable Modulo where
  eval eval _ t = traverse eval t >>= go where
    go (Modulo a b) = liftNumeric2 (liftIntegralFrac mod mod') a b

instance Tokenize Modulo where
  tokenize Modulo{..} = within' (Scope.InfixL Modulus 7) $ lhs *> yield Token.Sym <* rhs

data Power a = Power { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Power

instance Evaluatable Power where
  eval eval _ t = traverse eval t >>= go where
    go (Power a b) = liftNumeric2 liftedExponent a b

instance Tokenize Power where
  tokenize Power{..} = within' (Scope.InfixL Raise 9) $ lhs *> yield Token.Sym <* rhs

newtype Negate a = Negate { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Negate

instance Evaluatable Negate where
  eval eval _ t = traverse eval t >>= go where
    go (Negate a) = liftNumeric negate a

instance Tokenize Negate where
  tokenize Negate{..} = within' (Scope.Prefix NumericNegate) $ yield Token.Sym <* value

data FloorDivision a = FloorDivision { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FloorDivision

instance Evaluatable FloorDivision where
  eval eval _ t = traverse eval t >>= go where
    go (FloorDivision a b) = liftNumeric2 liftedFloorDiv a b

instance Tokenize FloorDivision where
  tokenize FloorDivision{..} = within' (Scope.InfixL FloorDivide 7) $ lhs *> yield Token.Sym <* rhs

-- | Regex matching operators (Ruby's =~ and ~!)
data Matches a = Matches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Matches

instance Evaluatable Matches

instance Tokenize Matches where
  tokenize Matches{..} = within' (Scope.InfixL RegexMatch 1) $ lhs *> yield Token.Sym <* rhs

data NotMatches a = NotMatches { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NotMatches

instance Evaluatable NotMatches

instance Tokenize NotMatches where
  tokenize NotMatches{..} = within' (Scope.InfixL RegexNotMatch 1) $ lhs *> yield Token.Sym <* rhs

data Or a = Or { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Or

instance Evaluatable Or where
  eval eval _ (Or a b) = do
    a' <- eval a
    ifthenelse a' (pure a') (eval b)

instance Tokenize Or where
  tokenize Or{..} = within' (Scope.InfixL LogicalOr 2) $ lhs *> yield Token.Sym <* rhs

data And a = And { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically And

instance Evaluatable And where
  eval eval _ (And a b) = do
    a' <- eval a
    ifthenelse a' (eval b) (pure a')

instance Tokenize And where
  tokenize And{..} = within' (Scope.InfixL LogicalAnd 2) $ lhs *> yield Token.Sym <* rhs

newtype Not a = Not { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Not

instance Evaluatable Not where
  eval eval _ (Not a) = eval a >>= asBool >>= boolean . not

instance Tokenize Not where
  tokenize Not{..} = within' (Scope.Prefix LogicalNot) $ yield Token.Sym <* value

data XOr a = XOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically XOr

instance Evaluatable XOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval eval _ (XOr a b) = liftA2 (/=) (eval a >>= asBool) (eval b >>= asBool) >>= boolean

instance Tokenize XOr where
  tokenize XOr{..} = within' (Scope.InfixL LogicalXor 2) $ lhs *> yield Token.Sym <* rhs

-- | Javascript delete operator
newtype Delete a = Delete { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Delete

instance Evaluatable Delete where
  eval _ ref (Delete a) = ref a >>= dealloc >> unit

-- | A sequence expression such as Javascript or C's comma operator.
data SequenceExpression a = SequenceExpression { firstExpression :: !a, secondExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically SequenceExpression

instance Evaluatable SequenceExpression where
  eval eval _ (SequenceExpression a b) =
    eval a >> eval b

-- | Javascript void operator
newtype Void a = Void { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Void

instance Evaluatable Void where
  eval eval _ (Void a) =
    eval a >> pure null

-- | Javascript typeof operator
newtype Typeof a = Typeof { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Typeof

-- TODO: Implement Eval instance for Typeof
instance Evaluatable Typeof

-- | Bitwise operators.
data BOr a = BOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BOr

instance Evaluatable BOr where
  eval eval _ (BOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.|.) a' b'

instance Tokenize BOr where
  tokenize BOr{..} = within' (Scope.InfixL BinaryOr 4) $ lhs *> yield Token.Sym <* rhs

data BAnd a = BAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BAnd

instance Evaluatable BAnd where
  eval eval _ (BAnd a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 (.&.) a' b'

instance Tokenize BAnd where
  tokenize BAnd{..} = within' (Scope.InfixL BinaryAnd 5) $ lhs *> yield Token.Sym <* rhs

data BXOr a = BXOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BXOr

instance Evaluatable BXOr where
  eval eval _ (BXOr a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    liftBitwise2 xor a' b'

instance Tokenize BXOr where
  tokenize BXOr{..} = within' (Scope.InfixL BinaryXor 5) $ lhs *> yield Token.Sym <* rhs

data LShift a = LShift { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LShift

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
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RShift

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
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically UnsignedRShift

instance Evaluatable UnsignedRShift where
  eval eval _ (UnsignedRShift a b) = do
    a' <- eval a >>= castToInteger
    b' <- eval b >>= castToInteger
    unsignedRShift a' b'

newtype Complement a = Complement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Complement

instance Evaluatable Complement where
  eval eval _ (Complement a) = do
    a' <- eval a >>= castToInteger
    liftBitwise complement a'

instance Tokenize Complement where
  tokenize Complement{..} = within' (Scope.Prefix BinaryComplement) $ yield Token.Sym <* value

-- | Member Access (e.g. a.b)
data MemberAccess a = MemberAccess { lhs :: a, rhs :: a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically MemberAccess

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


instance Tokenize MemberAccess where
  tokenize MemberAccess{..} = lhs *> yield Access <* rhs

-- | Subscript (e.g a[1])
data Subscript a = Subscript { lhs :: a, rhs :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Subscript

-- TODO: Finish Eval instance for Subscript
-- TODO return a special LvalSubscript instance here
instance Evaluatable Subscript where
  eval eval _ (Subscript l [r]) = join (index <$> eval l <*> eval r)
  eval _    _ (Subscript _ _)   = throwUnspecializedError (UnspecializedError "Eval unspecialized for subscript with slices")

instance Tokenize Subscript where
  tokenize Subscript{..} = lhs *> within' Scope.Indexing (sequenceA_ (intersperse (yield Token.Sep) rhs))

data Member a = Member { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Member

instance Evaluatable Member where

instance Tokenize Member where
  tokenize Member{..} = lhs *> yield Token.Access <* rhs

-- | Enumeration (e.g. a[1:10:1] in Python (start at index 1, stop at index 10, step 1 element from start to stop))
data Enumeration a = Enumeration { enumerationStart :: !a, enumerationEnd :: !a, enumerationStep :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Enumeration

-- TODO: Implement Eval instance for Enumeration
instance Evaluatable Enumeration

instance Tokenize Enumeration where
  tokenize Enumeration{..} = within Scope.Slice $ enumerationStart *> enumerationEnd *> enumerationStep

-- | InstanceOf (e.g. a instanceof b in JavaScript
data InstanceOf a = InstanceOf { instanceOfSubject :: !a, instanceOfObject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InstanceOf

-- TODO: Implement Eval instance for InstanceOf
instance Evaluatable InstanceOf


-- | ScopeResolution (e.g. import a.b in Python or a::b in C++)
newtype ScopeResolution a = ScopeResolution { scopes :: NonEmpty a }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ScopeResolution

instance Hashable1 ScopeResolution where liftHashWithSalt = foldl

instance Evaluatable ScopeResolution

instance Tokenize ScopeResolution where
  tokenize (ScopeResolution (a :| rest)) =
    a *> for_ rest (yield Token.Resolve *>)

instance Declarations1 ScopeResolution where
  liftDeclaredName declaredName = declaredName . NonEmpty.last . scopes

-- | A non-null expression such as Typescript or Swift's ! expression.
newtype NonNullExpression a = NonNullExpression { nonNullExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NonNullExpression

-- TODO: Implement Eval instance for NonNullExpression
instance Evaluatable NonNullExpression


-- | An await expression in Javascript or C#.
newtype Await a = Await { awaitSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Await
-- TODO: Improve this to model asynchrony or capture some data suggesting async calls are not a problem.
--       We are currently dealing with an asynchronous construct synchronously.
instance Evaluatable Await where
  eval eval _ (Await a) = eval a

-- | An object constructor call in Javascript, Java, etc.
data New a = New { newSubject :: a , newTypeParameters :: a, newArguments :: [a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically New

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
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Cast

instance Evaluatable Cast

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Super

instance Evaluatable Super

instance Tokenize Super where
  tokenize _ = yield Token.Superclass

data This a = This
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically This

instance Tokenize This where
  tokenize _ = yield Self

instance Evaluatable This where
  eval _ _ This = do
    span <- ask @Span
    reference (Reference __self) span ScopeGraph.This (Declaration __self)
    deref =<< lookupSlot (Declaration __self)

instance AccessControls1 This where
  liftTermToAccessControl _ _ = Just Private
