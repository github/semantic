{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Literal where

import Prelude hiding (Float, null)
import Prologue hiding (Set, hash, null)

import           Data.Abstract.Evaluatable as Eval
import           Data.JSON.Fields
import           Data.Scientific.Exts
import qualified Data.Text as T
import           Diffing.Algorithm
import           Numeric.Exts
import           Proto3.Suite.Class
import           Reprinting.Tokenize as Tok
import qualified Data.Reprinting.Scope as Scope
import           Text.Read (readMaybe)

-- Boolean

newtype Boolean a = Boolean { booleanContent :: Bool }
  deriving stock (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1)
  deriving anyclass (Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Boolean

true :: Boolean a
true = Boolean True

false :: Boolean a
false = Boolean False

instance Evaluatable Boolean where
  eval _ _ (Boolean x) = boolean x

instance Tokenize Boolean where
  tokenize = yield . Truth . booleanContent

-- | A literal integer of unspecified width. No particular base is implied.
newtype Integer a = Integer { integerContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Data.Syntax.Literal.Integer

instance Evaluatable Data.Syntax.Literal.Integer where
  -- TODO: We should use something more robust than shelling out to readMaybe.
  eval _ _ (Data.Syntax.Literal.Integer x) =
    either (const (throwEvalError (IntegerFormatError x))) pure (parseInteger x) >>= integer

instance Tokenize Data.Syntax.Literal.Integer where
  tokenize = yield . Run . integerContent

-- | A literal float of unspecified width.

newtype Float a = Float { floatContent :: Text }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Data.Syntax.Literal.Float


instance Evaluatable Data.Syntax.Literal.Float where
  eval _ _ (Float s) =
    either (const (throwEvalError (FloatFormatError s))) pure (parseScientific s) >>= float

instance Tokenize Data.Syntax.Literal.Float where
  tokenize = yield . Run . floatContent

-- Rational literals e.g. `2/3r`
newtype Rational a = Rational { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Data.Syntax.Literal.Rational

instance Evaluatable Data.Syntax.Literal.Rational where
  eval _ _ (Rational r) =
    let
      trimmed = T.takeWhile (/= 'r') r
      parsed = readMaybe @Prelude.Integer (T.unpack trimmed)
    in maybe (throwEvalError (RationalFormatError r)) (pure . toRational) parsed >>= rational

instance Tokenize Data.Syntax.Literal.Rational where
  tokenize (Rational t) = yield . Run $ t

-- Complex literals e.g. `3 + 2i`
newtype Complex a = Complex { value :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Complex

-- TODO: Implement Eval instance for Complex
instance Evaluatable Complex

instance Tokenize Complex where
  tokenize (Complex v) = yield . Run $ v

-- Strings, symbols

newtype String a = String { stringElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Data.Syntax.Literal.String

-- TODO: Should string literal bodies include escapes too?

-- TODO: Implement Eval instance for String
instance Evaluatable Data.Syntax.Literal.String

instance Tokenize Data.Syntax.Literal.String where
  tokenize = sequenceA_

newtype Character a = Character { characterContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Character

instance Evaluatable Data.Syntax.Literal.Character

instance Tokenize Character where
  tokenize = yield . Glyph . characterContent

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically InterpolationElement

-- TODO: Implement Eval instance for InterpolationElement
instance Evaluatable InterpolationElement

instance Tokenize InterpolationElement where
  tokenize = sequenceA_

-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically TextElement

instance Evaluatable TextElement where
  eval _ _ (TextElement x) = string x

instance Tokenize TextElement where
  tokenize = yield . Run . textElementContent

isTripleQuoted :: TextElement a -> Bool
isTripleQuoted (TextElement t) =
  let trip = "\"\"\""
  in  T.take 3 t == trip && T.takeEnd 3 t == trip

quoted :: Text -> TextElement a
quoted t = TextElement ("\"" <> t <> "\"")

-- | A sequence of textual contents within a string literal.
newtype EscapeSequence a = EscapeSequence { value :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically EscapeSequence

-- TODO: Implement Eval instance for EscapeSequence
instance Evaluatable EscapeSequence

instance Tokenize EscapeSequence where
  tokenize (EscapeSequence e) = yield . Run $ e

data Null a = Null
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Null

instance Evaluatable Null where eval _ _ _ = pure null

instance Tokenize Null where
  tokenize _ = yield Nullity

newtype Symbol a = Symbol { symbolElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Symbol

-- TODO: Implement Eval instance for Symbol
instance Evaluatable Symbol

instance Tokenize Symbol where
  tokenize s = within Scope.Atom (yield Sym *> sequenceA_ s)

newtype SymbolElement a = SymbolElement { symbolContent :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically SymbolElement

instance Evaluatable SymbolElement where
  eval _ _ (SymbolElement s) = string s

instance Tokenize SymbolElement where
  tokenize = yield . Run . symbolContent

newtype Regex a = Regex { regexContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Regex

-- TODO: Heredoc-style string literals?

-- TODO: Implement Eval instance for Regex
instance Evaluatable Regex where
  eval _ _ (Regex x) = string x

instance Tokenize Regex where
  tokenize = yield . Run . regexContent

-- Collections

newtype Array a = Array { arrayElements :: [a] }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Array

instance Evaluatable Array where
  eval eval _ Array{..} = array =<< traverse eval arrayElements

instance Tokenize Array where
  tokenize = list . arrayElements

newtype Hash a = Hash { hashElements :: [a] }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Hash

instance Evaluatable Hash where
  eval eval _ t = do
    elements <- traverse (eval >=> asPair) (hashElements t)
    Eval.hash elements

instance Tokenize Hash where
  tokenize = Tok.hash . hashElements

data KeyValue a = KeyValue { key :: !a, value :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically KeyValue

instance Evaluatable KeyValue where
  eval eval _ (KeyValue{..}) = do
    k <- eval key
    v <- eval value
    kvPair k v

instance Tokenize KeyValue where
  tokenize (KeyValue k v) = pair k v

newtype Tuple a = Tuple { tupleContents :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Tuple

instance Evaluatable Tuple where
  eval eval _ (Tuple cs) = tuple =<< traverse eval cs

newtype Set a = Set { setElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Set

-- TODO: Implement Eval instance for Set
instance Evaluatable Set


-- Pointers

-- | A declared pointer (e.g. var pointer *int in Go)
newtype Pointer a = Pointer { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Pointer

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer


-- | A reference to a pointer's address (e.g. &pointer in Go)
newtype Reference a = Reference { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Ord1, Show1) via Generically Reference

-- TODO: Implement Eval instance for Reference
instance Evaluatable Reference

-- TODO: Object literals as distinct from hash literals? Or coalesce object/hash literals into “key-value literals”?
-- TODO: Function literals (lambdas, procs, anonymous functions, what have you).
