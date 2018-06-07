{-# LANGUAGE DeriveAnyClass #-}
module Language.Haskell.Syntax where

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Prelude
import           Prologue

data Module a = Module { moduleIdentifier :: !a
                       , moduleExports    :: ![a]
                       , moduleStatements :: !a
                       }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module

data StrictType a = StrictType { strictTypeIdentifier :: !a, strictTypeParameters :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 StrictType where liftEq = genericLiftEq
instance Ord1 StrictType where liftCompare = genericLiftCompare
instance Show1 StrictType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictType

newtype StrictTypeVariable a = StrictTypeVariable { strictTypeVariableIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 StrictTypeVariable where liftEq = genericLiftEq
instance Ord1 StrictTypeVariable where liftCompare = genericLiftCompare
instance Show1 StrictTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictTypeVariable

data Type a = Type { typeIdentifier :: a, typeParameters :: a, typeKindSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: !a, typeSynonymRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeSynonym where liftEq = genericLiftEq
instance Ord1 TypeSynonym where liftCompare = genericLiftCompare
instance Show1 TypeSynonym where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeSynonym

data UnitConstructor a = UnitConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 UnitConstructor where liftEq = genericLiftEq
instance Ord1 UnitConstructor where liftCompare = genericLiftCompare
instance Show1 UnitConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable UnitConstructor

newtype TupleConstructor a = TupleConstructor { tupleConstructorArity :: Int }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TupleConstructor where liftEq = genericLiftEq
instance Ord1 TupleConstructor where liftCompare = genericLiftCompare
instance Show1 TupleConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ListConstructor where liftEq = genericLiftEq
instance Ord1 ListConstructor where liftCompare = genericLiftCompare
instance Show1 ListConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FunctionConstructor where liftEq = genericLiftEq
instance Ord1 FunctionConstructor where liftCompare = genericLiftCompare
instance Show1 FunctionConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionConstructor

data RecordDataConstructor a = RecordDataConstructor { recordDataConstructorName :: !a, recordDataConstructorFields :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 RecordDataConstructor where liftEq = genericLiftEq
instance Ord1 RecordDataConstructor where liftCompare = genericLiftCompare
instance Show1 RecordDataConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RecordDataConstructor

data Field a = Field { fieldName :: !a, fieldBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Field where liftEq = genericLiftEq
instance Ord1 Field where liftCompare = genericLiftCompare
instance Show1 Field where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Field

newtype Pragma a = Pragma Text
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Pragma where liftEq = genericLiftEq
instance Ord1 Pragma where liftCompare = genericLiftCompare
instance Show1 Pragma where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Pragma

newtype Deriving a = Deriving [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Deriving where liftEq = genericLiftEq
instance Ord1 Deriving where liftCompare = genericLiftCompare
instance Show1 Deriving where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Deriving

newtype Context' a = Context' [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Context' where liftEq = genericLiftEq
instance Ord1 Context' where liftCompare = genericLiftCompare
instance Show1 Context' where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Context'

data Class a = Class { classType :: a, classTypeParameters :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class

data GADT a = GADT { gadtContext :: a, gadtName :: a, gadtConstructors :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 GADT where liftEq = genericLiftEq
instance Ord1 GADT where liftCompare = genericLiftCompare
instance Show1 GADT where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GADT

data GADTConstructor a = GADTConstructor { gadtConstructorContext :: a, gadtConstructorName :: a, gadtConstructorTypeSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 GADTConstructor where liftEq = genericLiftEq
instance Ord1 GADTConstructor where liftCompare = genericLiftCompare
instance Show1 GADTConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GADTConstructor

data FunctionType a = FunctionType { functionTypeLeft :: a, functionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionType

data TypeSignature a = TypeSignature { typeSignatureName :: a, typeSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeSignature where liftEq = genericLiftEq
instance Ord1 TypeSignature where liftCompare = genericLiftCompare
instance Show1 TypeSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeSignature

newtype KindSignature a = KindSignature { kindSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 KindSignature where liftEq = genericLiftEq
instance Ord1 KindSignature where liftCompare = genericLiftCompare
instance Show1 KindSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindSignature

data KindFunctionType a = KindFunctionType { kindFunctionTypeLeft :: a, kindFunctionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 KindFunctionType where liftEq = genericLiftEq
instance Ord1 KindFunctionType where liftCompare = genericLiftCompare
instance Show1 KindFunctionType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindFunctionType

newtype Kind a = Kind { kindKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Kind where liftEq = genericLiftEq
instance Ord1 Kind where liftCompare = genericLiftCompare
instance Show1 Kind where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Kind

newtype KindListType a = KindListType { kindListTypeKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 KindListType where liftEq = genericLiftEq
instance Ord1 KindListType where liftCompare = genericLiftCompare
instance Show1 KindListType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindListType

data Star a = Star
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Star where liftEq = genericLiftEq
instance Ord1 Star where liftCompare = genericLiftCompare
instance Show1 Star where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Star

newtype QualifiedTypeConstructorIdentifier a = QualifiedTypeConstructorIdentifier { qualifiedTypeConstructorIdentifierName :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedTypeConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedTypeConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedTypeConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedTypeConstructorIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedTypeConstructorIdentifier

data AnnotatedTypeVariable a = AnnotatedTypeVariable { annotatedTypeVariableIdentifier :: a, annotatedTypeVariableannotation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AnnotatedTypeVariable where liftEq = genericLiftEq
instance Ord1 AnnotatedTypeVariable where liftCompare = genericLiftCompare
instance Show1 AnnotatedTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AnnotatedTypeVariable

newtype QualifiedModuleIdentifier a = QualifiedModuleIdentifier { qualifiedModuleIdentifierName :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedModuleIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedModuleIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedModuleIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedModuleIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedModuleIdentifier

newtype Export a = Export { exportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Export where liftEq = genericLiftEq
instance Ord1 Export where liftCompare = genericLiftCompare
instance Show1 Export where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Export

newtype ModuleExport a = ModuleExport { moduleExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ModuleExport where liftEq = genericLiftEq
instance Ord1 ModuleExport where liftCompare = genericLiftCompare
instance Show1 ModuleExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ModuleExport

newtype TypeConstructorExport a = TypeConstructorExport { typeConstructorExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeConstructorExport where liftEq = genericLiftEq
instance Ord1 TypeConstructorExport where liftCompare = genericLiftCompare
instance Show1 TypeConstructorExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeConstructorExport

newtype VariableOperator a = VariableOperator { variableOperatorContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 VariableOperator where liftEq = genericLiftEq
instance Ord1 VariableOperator where liftCompare = genericLiftCompare
instance Show1 VariableOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableOperator

data AllConstructors a = AllConstructors
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AllConstructors where liftEq = genericLiftEq
instance Ord1 AllConstructors where liftCompare = genericLiftCompare
instance Show1 AllConstructors where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AllConstructors

newtype ConstructorOperator a = ConstructorOperator { constructorOperatorContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ConstructorOperator where liftEq = genericLiftEq
instance Ord1 ConstructorOperator where liftCompare = genericLiftCompare
instance Show1 ConstructorOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorOperator
