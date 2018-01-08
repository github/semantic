{-# LANGUAGE DeriveAnyClass #-}
module Language.PHP.Syntax where

import Diffing.Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Generic
import Data.Mergeable
import GHC.Generics

newtype Text a = Text ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Ord1 Text where liftCompare = genericLiftCompare
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

newtype VariableName a = VariableName a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 VariableName where liftEq = genericLiftEq
instance Ord1 VariableName where liftCompare = genericLiftCompare
instance Show1 VariableName where liftShowsPrec = genericLiftShowsPrec

newtype RequireOnce a = RequireOnce a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RequireOnce where liftEq = genericLiftEq
instance Ord1 RequireOnce where liftCompare = genericLiftCompare
instance Show1 RequireOnce where liftShowsPrec = genericLiftShowsPrec

newtype Require a = Require a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Require where liftEq          = genericLiftEq
instance Ord1 Require where liftCompare    = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

newtype Include a = Include a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Include where liftEq          = genericLiftEq
instance Ord1 Include where liftCompare    = genericLiftCompare
instance Show1 Include where liftShowsPrec = genericLiftShowsPrec

newtype IncludeOnce a = IncludeOnce a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 IncludeOnce where liftEq          = genericLiftEq
instance Ord1 IncludeOnce where liftCompare    = genericLiftCompare
instance Show1 IncludeOnce where liftShowsPrec = genericLiftShowsPrec

newtype ArrayElement a = ArrayElement a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ArrayElement where liftEq          = genericLiftEq
instance Ord1 ArrayElement where liftCompare    = genericLiftCompare
instance Show1 ArrayElement where liftShowsPrec = genericLiftShowsPrec

newtype GlobalDeclaration a = GlobalDeclaration [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 GlobalDeclaration where liftEq          = genericLiftEq
instance Ord1 GlobalDeclaration where liftCompare    = genericLiftCompare
instance Show1 GlobalDeclaration where liftShowsPrec = genericLiftShowsPrec

newtype SimpleVariable a = SimpleVariable a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 SimpleVariable where liftEq          = genericLiftEq
instance Ord1 SimpleVariable where liftCompare    = genericLiftCompare
instance Show1 SimpleVariable where liftShowsPrec = genericLiftShowsPrec


-- | TODO: Unify with TypeScript's PredefinedType
newtype CastType a = CastType { _castType :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 CastType where liftEq = genericLiftEq
instance Ord1 CastType where liftCompare = genericLiftCompare
instance Show1 CastType where liftShowsPrec = genericLiftShowsPrec

newtype ErrorControl a = ErrorControl a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ErrorControl where liftEq = genericLiftEq
instance Ord1 ErrorControl where liftCompare = genericLiftCompare
instance Show1 ErrorControl where liftShowsPrec = genericLiftShowsPrec

newtype Clone a = Clone a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Clone where liftEq = genericLiftEq
instance Ord1 Clone where liftCompare = genericLiftCompare
instance Show1 Clone where liftShowsPrec = genericLiftShowsPrec

newtype ShellCommand a = ShellCommand ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ShellCommand where liftEq = genericLiftEq
instance Ord1 ShellCommand where liftCompare = genericLiftCompare
instance Show1 ShellCommand where liftShowsPrec = genericLiftShowsPrec

-- | TODO: Combine with TypeScript update expression.
newtype Update a = Update { _updateSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec

newtype NewVariable a = NewVariable [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NewVariable where liftEq = genericLiftEq
instance Ord1 NewVariable where liftCompare = genericLiftCompare
instance Show1 NewVariable where liftShowsPrec = genericLiftShowsPrec

newtype RelativeScope a = RelativeScope ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RelativeScope where liftEq = genericLiftEq
instance Ord1 RelativeScope where liftCompare = genericLiftCompare
instance Show1 RelativeScope where liftShowsPrec = genericLiftShowsPrec

data QualifiedName a = QualifiedName a a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 QualifiedName where liftEq = genericLiftEq
instance Ord1 QualifiedName where liftCompare = genericLiftCompare
instance Show1 QualifiedName where liftShowsPrec = genericLiftShowsPrec

data NamespaceName a = NamespaceName [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceName where liftEq = genericLiftEq
instance Ord1 NamespaceName where liftCompare = genericLiftCompare
instance Show1 NamespaceName where liftShowsPrec = genericLiftShowsPrec

data ConstDeclaration a = ConstDeclaration [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ConstDeclaration where liftEq = genericLiftEq
instance Ord1 ConstDeclaration where liftCompare = genericLiftCompare
instance Show1 ConstDeclaration where liftShowsPrec = genericLiftShowsPrec

data ClassConstDeclaration a = ClassConstDeclaration a [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ClassConstDeclaration where liftEq = genericLiftEq
instance Ord1 ClassConstDeclaration where liftCompare = genericLiftCompare
instance Show1 ClassConstDeclaration where liftShowsPrec = genericLiftShowsPrec

data ClassInterfaceClause a = ClassInterfaceClause [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ClassInterfaceClause where liftEq = genericLiftEq
instance Ord1 ClassInterfaceClause where liftCompare = genericLiftCompare
instance Show1 ClassInterfaceClause where liftShowsPrec = genericLiftShowsPrec

data ClassBaseClause a = ClassBaseClause a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ClassBaseClause where liftEq = genericLiftEq
instance Ord1 ClassBaseClause where liftCompare = genericLiftCompare
instance Show1 ClassBaseClause where liftShowsPrec = genericLiftShowsPrec


data UseClause a = UseClause [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 UseClause where liftEq = genericLiftEq
instance Ord1 UseClause where liftCompare = genericLiftCompare
instance Show1 UseClause where liftShowsPrec = genericLiftShowsPrec

data ReturnType a = ReturnType a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ReturnType where liftEq = genericLiftEq
instance Ord1 ReturnType where liftCompare = genericLiftCompare
instance Show1 ReturnType where liftShowsPrec = genericLiftShowsPrec

data TypeDeclaration a = TypeDeclaration a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeDeclaration where liftEq = genericLiftEq
instance Ord1 TypeDeclaration where liftCompare = genericLiftCompare
instance Show1 TypeDeclaration where liftShowsPrec = genericLiftShowsPrec

data BaseTypeDeclaration a = BaseTypeDeclaration a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 BaseTypeDeclaration where liftEq = genericLiftEq
instance Ord1 BaseTypeDeclaration where liftCompare = genericLiftCompare
instance Show1 BaseTypeDeclaration where liftShowsPrec = genericLiftShowsPrec

data ScalarType a = ScalarType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ScalarType where liftEq = genericLiftEq
instance Ord1 ScalarType where liftCompare = genericLiftCompare
instance Show1 ScalarType where liftShowsPrec = genericLiftShowsPrec

data EmptyIntrinsic a = EmptyIntrinsic a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 EmptyIntrinsic where liftEq = genericLiftEq
instance Ord1 EmptyIntrinsic where liftCompare = genericLiftCompare
instance Show1 EmptyIntrinsic where liftShowsPrec = genericLiftShowsPrec

data ExitIntrinsic a = ExitIntrinsic a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ExitIntrinsic where liftEq = genericLiftEq
instance Ord1 ExitIntrinsic where liftCompare = genericLiftCompare
instance Show1 ExitIntrinsic where liftShowsPrec = genericLiftShowsPrec

data IssetIntrinsic a = IssetIntrinsic a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 IssetIntrinsic where liftEq = genericLiftEq
instance Ord1 IssetIntrinsic where liftCompare = genericLiftCompare
instance Show1 IssetIntrinsic where liftShowsPrec = genericLiftShowsPrec

data EvalIntrinsic a = EvalIntrinsic a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 EvalIntrinsic where liftEq = genericLiftEq
instance Ord1 EvalIntrinsic where liftCompare = genericLiftCompare
instance Show1 EvalIntrinsic where liftShowsPrec = genericLiftShowsPrec

data PrintIntrinsic a = PrintIntrinsic a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 PrintIntrinsic where liftEq = genericLiftEq
instance Ord1 PrintIntrinsic where liftCompare = genericLiftCompare
instance Show1 PrintIntrinsic where liftShowsPrec = genericLiftShowsPrec

data NamespaceAliasingClause a = NamespaceAliasingClause a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceAliasingClause where liftEq = genericLiftEq
instance Ord1 NamespaceAliasingClause where liftCompare = genericLiftCompare
instance Show1 NamespaceAliasingClause where liftShowsPrec = genericLiftShowsPrec

newtype NamespaceUseDeclaration a = NamespaceUseDeclaration [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceUseDeclaration where liftEq = genericLiftEq
instance Ord1 NamespaceUseDeclaration where liftCompare = genericLiftCompare
instance Show1 NamespaceUseDeclaration where liftShowsPrec = genericLiftShowsPrec

newtype NamespaceUseClause a = NamespaceUseClause [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceUseClause where liftEq = genericLiftEq
instance Ord1 NamespaceUseClause where liftCompare = genericLiftCompare
instance Show1 NamespaceUseClause where liftShowsPrec = genericLiftShowsPrec

newtype NamespaceUseGroupClause a = NamespaceUseGroupClause [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceUseGroupClause where liftEq = genericLiftEq
instance Ord1 NamespaceUseGroupClause where liftCompare = genericLiftCompare
instance Show1 NamespaceUseGroupClause where liftShowsPrec = genericLiftShowsPrec
