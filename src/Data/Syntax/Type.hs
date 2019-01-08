{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingVia, DuplicateRecordFields, DeriveGeneric, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Type where

import Data.Abstract.Evaluatable hiding (Void)
import Data.JSON.Fields
import Diffing.Algorithm
import Prelude hiding (Bool, Float, Int, Double)
import Prologue hiding (Map)
import Proto3.Suite.Class
import Reprinting.Tokenize

data Array a = Array { arraySize :: Maybe a, arrayElementType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Array

instance Named1 Array where nameOf1 _ = "TypeArray"


-- TODO: Implement Eval instance for Array
instance Evaluatable Array


-- TODO: What about type variables? re: FreeVariables1
data Annotation a = Annotation { annotationSubject :: a, annotationType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Annotation

-- TODO: Specialize Evaluatable for Type to unify the inferred type of the subject with the specified type
instance Evaluatable Annotation where
  eval eval _ Annotation{..} = eval annotationSubject

instance Tokenize Annotation where
  -- FIXME: This ignores annotationType.
  -- TODO: Not sure what this should look like yet
  tokenize Annotation{..} = annotationSubject


data Function a = Function { functionParameters :: [a], functionReturn :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Function

instance Named1 Function where nameOf1 _ = "TypeFunction"

-- TODO: Implement Eval instance for Function
instance Evaluatable Function


newtype Interface a = Interface { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Interface

-- TODO: Implement Eval instance for Interface
instance Evaluatable Interface

data Map a = Map { mapKeyType :: a, mapElementType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Map

-- TODO: Implement Eval instance for Map
instance Evaluatable Map

newtype Parenthesized a = Parenthesized { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Parenthesized

-- TODO: Implement Eval instance for Parenthesized
instance Evaluatable Parenthesized

newtype Pointer a = Pointer { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Pointer

instance Named1 Pointer where nameOf1 _ = "TypePointer"

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer

newtype Product a = Product { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Product

-- TODO: Implement Eval instance for Product
instance Evaluatable Product


data Readonly a = Readonly
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Readonly

-- TODO: Implement Eval instance for Readonly
instance Evaluatable Readonly

newtype Slice a = Slice { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Slice

instance Named1 Slice where nameOf1 _ = "TypeSlice"

-- TODO: Implement Eval instance for Slice
instance Evaluatable Slice

newtype TypeParameters a = TypeParameters { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeParameters

-- TODO: Implement Eval instance for TypeParameters
instance Evaluatable TypeParameters

-- data instead of newtype because no payload
data Void a = Void
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Void

-- TODO: Implement Eval instance for Void
instance Evaluatable Void

-- data instead of newtype because no payload
data Int a = Int
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Int

-- TODO: Implement Eval instance for Int
instance Evaluatable Int

data Float a = Float
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Float

instance Named1 Float where nameOf1 _ = "TypeFloat"

-- TODO: Implement Eval instance for Float
instance Evaluatable Float

data Double a = Double
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Double

-- TODO: Implement Eval instance for Double
instance Evaluatable Double

data Bool a = Bool
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Bool

-- TODO: Implement Eval instance for Float
instance Evaluatable Bool
