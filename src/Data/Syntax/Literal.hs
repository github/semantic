{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Literal where

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import           Data.Scientific.Exts
import qualified Data.Text as T
import           Diffing.Algorithm
import           Numeric.Exts
import           Prelude hiding (Float, null)
import           Prologue hiding (Set, hash, null)
import           Proto3.Suite.Class
import           Reprinting.Tokenize
import           Text.Read (readMaybe)

-- Boolean

newtype Boolean a = Boolean { booleanContent :: Bool }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

true :: Boolean a
true = Boolean True

false :: Boolean a
false = Boolean False

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Boolean where
  eval (Boolean x) = rvalBox (boolean x)

instance Tokenize Boolean where
  tokenize = yield . Truth . booleanContent

-- Numeric

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Data.Syntax.Literal.Integer where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Integer where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Integer where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Integer where
  -- TODO: We should use something more robust than shelling out to readMaybe.
  eval (Data.Syntax.Literal.Integer x) =
    rvalBox =<< (integer <$> either (const (throwEvalError (IntegerFormatError x))) pure (parseInteger x))

instance Tokenize Data.Syntax.Literal.Integer where
  tokenize = yield . Fragment . integerContent

-- | A literal float of unspecified width.

newtype Float a = Float { floatContent :: Text }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Data.Syntax.Literal.Float where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Float where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Float where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Float where
  eval (Float s) =
    rvalBox =<< (float <$> either (const (throwEvalError (FloatFormatError s))) pure (parseScientific s))

instance Tokenize Data.Syntax.Literal.Float where
  tokenize = yield . Fragment . floatContent

-- Rational literals e.g. `2/3r`
newtype Rational a = Rational { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Data.Syntax.Literal.Rational where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Rational where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Rational where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Rational where
  eval (Rational r) =
    let
      trimmed = T.takeWhile (/= 'r') r
      parsed = readMaybe @Prelude.Integer (T.unpack trimmed)
    in rvalBox =<< (rational <$> maybe (throwEvalError (RationalFormatError r)) (pure . toRational) parsed)

-- Complex literals e.g. `3 + 2i`
newtype Complex a = Complex { value :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Data.Syntax.Literal.Complex where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Complex where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Complex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Complex
instance Evaluatable Complex

-- Strings, symbols

newtype String a = String { stringElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Data.Syntax.Literal.String where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.String where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.String where liftShowsPrec = genericLiftShowsPrec

-- TODO: Should string literal bodies include escapes too?

-- TODO: Implement Eval instance for String
instance Evaluatable Data.Syntax.Literal.String

newtype Character a = Character { characterContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Data.Syntax.Literal.Character where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Character where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Character where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Character

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 InterpolationElement where liftEq = genericLiftEq
instance Ord1 InterpolationElement where liftCompare = genericLiftCompare
instance Show1 InterpolationElement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterpolationElement
instance Evaluatable InterpolationElement

-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 TextElement where liftEq = genericLiftEq
instance Ord1 TextElement where liftCompare = genericLiftCompare
instance Show1 TextElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TextElement where
  eval (TextElement x) = rvalBox (string x)

instance Tokenize TextElement where
  tokenize = yield . Fragment . textElementContent

-- | A sequence of textual contents within a string literal.
newtype EscapeSequence a = EscapeSequence { value :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 EscapeSequence where liftEq = genericLiftEq
instance Ord1 EscapeSequence where liftCompare = genericLiftCompare
instance Show1 EscapeSequence where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for EscapeSequence
instance Evaluatable EscapeSequence

data Null a = Null
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Null where liftEq = genericLiftEq
instance Ord1 Null where liftCompare = genericLiftCompare
instance Show1 Null where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Null where eval _ = rvalBox null

instance Tokenize Null where
  tokenize _ = yield Nullity

newtype Symbol a = Symbol { symbolElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Symbol where liftEq = genericLiftEq
instance Ord1 Symbol where liftCompare = genericLiftCompare
instance Show1 Symbol where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Symbol
instance Evaluatable Symbol

newtype SymbolElement a = SymbolElement { symbolContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 SymbolElement where liftEq = genericLiftEq
instance Ord1 SymbolElement where liftCompare = genericLiftCompare
instance Show1 SymbolElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SymbolElement where
  eval (SymbolElement s) = rvalBox (symbol s)

newtype Regex a = Regex { regexContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Regex where liftEq = genericLiftEq
instance Ord1 Regex where liftCompare = genericLiftCompare
instance Show1 Regex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Heredoc-style string literals?

-- TODO: Implement Eval instance for Regex
instance Evaluatable Regex where
  eval (Regex x) = rvalBox (regex x)

-- Collections

newtype Array a = Array { arrayElements :: [a] }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Array where
  eval (Array a) = rvalBox =<< array =<< traverse subtermAddress a

instance Tokenize Array where
  tokenize = list_ . arrayElements

newtype Hash a = Hash { hashElements :: [a] }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Hash where liftEq = genericLiftEq
instance Ord1 Hash where liftCompare = genericLiftCompare
instance Show1 Hash where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Hash where
  eval t = rvalBox =<< (hash <$> traverse (subtermValue >=> asPair) (hashElements t))

instance Tokenize Hash where
  tokenize = hash_ . hashElements

data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 KeyValue where liftEq = genericLiftEq
instance Ord1 KeyValue where liftCompare = genericLiftCompare
instance Show1 KeyValue where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KeyValue where
  eval (fmap subtermValue -> KeyValue{..}) =
    rvalBox =<< (kvPair <$> key <*> value)

instance Tokenize KeyValue where
  tokenize (KeyValue k v) = pair_ k v

newtype Tuple a = Tuple { tupleContents :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Tuple where
  eval (Tuple cs) = rvalBox =<< tuple =<< traverse subtermAddress cs

newtype Set a = Set { setElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Set where liftEq = genericLiftEq
instance Ord1 Set where liftCompare = genericLiftCompare
instance Show1 Set where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Set
instance Evaluatable Set


-- Pointers

-- | A declared pointer (e.g. var pointer *int in Go)
newtype Pointer a = Pointer a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer


-- | A reference to a pointer's address (e.g. &pointer in Go)
newtype Reference a = Reference a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Reference where liftEq = genericLiftEq
instance Ord1 Reference where liftCompare = genericLiftCompare
instance Show1 Reference where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Reference
instance Evaluatable Reference

-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
