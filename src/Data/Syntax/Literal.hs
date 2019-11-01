{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}
module Data.Syntax.Literal (module Data.Syntax.Literal) where

import Prelude hiding (Float, null)
import Prologue hiding (Set, hash, null)

import           Data.Abstract.Evaluatable as Eval
import           Data.JSON.Fields
import           Data.Scientific.Exts
import qualified Data.Text as T
import           Diffing.Algorithm
import           Numeric.Exts
import           Text.Read (readMaybe)

-- Boolean

newtype Boolean a = Boolean { booleanContent :: Bool }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Boolean where liftEq = genericLiftEq
instance Ord1 Boolean where liftCompare = genericLiftCompare
instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec

true :: Boolean a
true = Boolean True

false :: Boolean a
false = Boolean False

instance Evaluatable Boolean where
  eval _ _ (Boolean x) = boolean x

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Data.Syntax.Literal.Integer where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Integer where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Integer where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Integer where
  -- TODO: We should use something more robust than shelling out to readMaybe.
  eval _ _ (Data.Syntax.Literal.Integer x) =
    either (const (throwEvalError (IntegerFormatError x))) pure (parseInteger x) >>= integer

-- | A literal float of unspecified width.

newtype Float a = Float { floatContent :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Data.Syntax.Literal.Float where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Float where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Float where liftShowsPrec = genericLiftShowsPrec


instance Evaluatable Data.Syntax.Literal.Float where
  eval _ _ (Float s) =
    either (const (throwEvalError (FloatFormatError s))) pure (parseScientific s) >>= float

-- Rational literals e.g. `2/3r`
newtype Rational a = Rational { value :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Data.Syntax.Literal.Rational where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.Rational where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.Rational where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Rational where
  eval _ _ (Rational r) =
    let
      trimmed = T.takeWhile (/= 'r') r
      parsed = readMaybe @Prelude.Integer (T.unpack trimmed)
    in maybe (throwEvalError (RationalFormatError r)) (pure . toRational) parsed >>= rational

-- Complex literals e.g. `3 + 2i`
newtype Complex a = Complex { value :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Complex where liftEq = genericLiftEq
instance Ord1 Complex where liftCompare = genericLiftCompare
instance Show1 Complex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Complex
instance Evaluatable Complex

-- Strings, symbols

newtype String a = String { stringElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Data.Syntax.Literal.String where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Literal.String where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Literal.String where liftShowsPrec = genericLiftShowsPrec

-- TODO: Should string literal bodies include escapes too?

-- TODO: Implement Eval instance for String
instance Evaluatable Data.Syntax.Literal.String

newtype Character a = Character { characterContent :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Character where liftEq = genericLiftEq
instance Ord1 Character where liftCompare = genericLiftCompare
instance Show1 Character where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Data.Syntax.Literal.Character

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InterpolationElement where liftEq = genericLiftEq
instance Ord1 InterpolationElement where liftCompare = genericLiftCompare
instance Show1 InterpolationElement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterpolationElement
instance Evaluatable InterpolationElement

-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 TextElement where liftEq = genericLiftEq
instance Ord1 TextElement where liftCompare = genericLiftCompare
instance Show1 TextElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TextElement where
  eval _ _ (TextElement x) = string x

isTripleQuoted :: TextElement a -> Bool
isTripleQuoted (TextElement t) =
  let trip = "\"\"\""
  in  T.take 3 t == trip && T.takeEnd 3 t == trip

quoted :: Text -> TextElement a
quoted t = TextElement ("\"" <> t <> "\"")

-- | A sequence of textual contents within a string literal.
newtype EscapeSequence a = EscapeSequence { value :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 EscapeSequence where liftEq = genericLiftEq
instance Ord1 EscapeSequence where liftCompare = genericLiftCompare
instance Show1 EscapeSequence where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for EscapeSequence
instance Evaluatable EscapeSequence

data Null a = Null
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Null where liftEq = genericLiftEq
instance Ord1 Null where liftCompare = genericLiftCompare
instance Show1 Null where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Null where eval _ _ _ = pure null

newtype Symbol a = Symbol { symbolElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Symbol where liftEq = genericLiftEq
instance Ord1 Symbol where liftCompare = genericLiftCompare
instance Show1 Symbol where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Symbol
instance Evaluatable Symbol

newtype SymbolElement a = SymbolElement { symbolContent :: Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 SymbolElement where liftEq = genericLiftEq
instance Ord1 SymbolElement where liftCompare = genericLiftCompare
instance Show1 SymbolElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SymbolElement where
  eval _ _ (SymbolElement s) = string s

newtype Regex a = Regex { regexContent :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Regex where liftEq = genericLiftEq
instance Ord1 Regex where liftCompare = genericLiftCompare
instance Show1 Regex where liftShowsPrec = genericLiftShowsPrec

-- TODO: Heredoc-style string literals?

-- TODO: Implement Eval instance for Regex
instance Evaluatable Regex where
  eval _ _ (Regex x) = string x

-- Collections

newtype Array a = Array { arrayElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Array where
  eval eval _ Array{..} = array =<< traverse eval arrayElements

newtype Hash a = Hash { hashElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Hash where liftEq = genericLiftEq
instance Ord1 Hash where liftCompare = genericLiftCompare
instance Show1 Hash where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Hash where
  eval eval _ t = do
    elements <- traverse (eval >=> asPair) (hashElements t)
    Eval.hash elements

data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 KeyValue where liftEq = genericLiftEq
instance Ord1 KeyValue where liftCompare = genericLiftCompare
instance Show1 KeyValue where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KeyValue where
  eval eval _ KeyValue{..} = do
    k <- eval key
    v <- eval value
    kvPair k v

newtype Tuple a = Tuple { tupleContents :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Tuple where
  eval eval _ (Tuple cs) = tuple =<< traverse eval cs

newtype Set a = Set { setElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Set where liftEq = genericLiftEq
instance Ord1 Set where liftCompare = genericLiftCompare
instance Show1 Set where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Set
instance Evaluatable Set


-- Pointers

-- | A declared pointer (e.g. var pointer *int in Go)
newtype Pointer a = Pointer { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer


-- | A reference to a pointer's address (e.g. &pointer in Go)
newtype Reference a = Reference { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Reference where liftEq = genericLiftEq
instance Ord1 Reference where liftCompare = genericLiftCompare
instance Show1 Reference where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Reference
instance Evaluatable Reference

-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
