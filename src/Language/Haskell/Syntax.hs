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

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: a, typeSynonymContext :: [a], typeSynonymRight :: a }
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
newtype Context' a = Context' a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Context' where liftEq = genericLiftEq
instance Ord1 Context' where liftCompare = genericLiftCompare
instance Show1 Context' where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Context'

newtype Class a = Class { classContent :: [a] }
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

data TypeSignature a = TypeSignature { typeSignatureName :: [a], typeSignatureContext :: [a], typeSignatureContent :: a }
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

data QualifiedEntityIdentifier a = QualifiedTypeConstructorIdentifier (NonEmpty a)
                                 | QualifiedConstructorIdentifier (NonEmpty a)
                                 | QualifiedInfixVariableIdentifier (NonEmpty a)
                                 | QualifiedModuleIdentifier (NonEmpty a)
                                 | QualifiedVariableIdentifier (NonEmpty a)
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedEntityIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedEntityIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedEntityIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedEntityIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedEntityIdentifier

data AnnotatedTypeVariable a = AnnotatedTypeVariable { annotatedTypeVariableIdentifier :: a, annotatedTypeVariableannotation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AnnotatedTypeVariable where liftEq = genericLiftEq
instance Ord1 AnnotatedTypeVariable where liftCompare = genericLiftCompare
instance Show1 AnnotatedTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AnnotatedTypeVariable

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

data AllConstructors a = AllConstructors
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AllConstructors where liftEq = genericLiftEq
instance Ord1 AllConstructors where liftCompare = genericLiftCompare
instance Show1 AllConstructors where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AllConstructors

data InfixOperatorPattern a = InfixOperatorPattern { infixOperatorPatternLeft :: a, infixOperatorPatternOperator :: a, infixOperatorPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 InfixOperatorPattern where liftEq = genericLiftEq
instance Ord1 InfixOperatorPattern where liftCompare = genericLiftCompare
instance Show1 InfixOperatorPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixOperatorPattern

newtype QuotedName a = QuotedName { quotedNameContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuotedName where liftEq = genericLiftEq
instance Ord1 QuotedName where liftCompare = genericLiftCompare
instance Show1 QuotedName where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuotedName

newtype TypePattern a = TypePattern { typePatternContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypePattern where liftEq = genericLiftEq
instance Ord1 TypePattern where liftCompare = genericLiftCompare
instance Show1 TypePattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypePattern

newtype ScopedTypeVariables a = ScopedTypeVariables { scopedTypeVariablesContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ScopedTypeVariables where liftEq = genericLiftEq
instance Ord1 ScopedTypeVariables where liftCompare = genericLiftCompare
instance Show1 ScopedTypeVariables where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ScopedTypeVariables

data NewType a = NewType { newTypeContext :: [a], newTypeLeft :: a, newTypeRight :: a, newTypeDeriving :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NewType where liftEq = genericLiftEq
instance Ord1 NewType where liftCompare = genericLiftCompare
instance Show1 NewType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NewType

newtype DefaultDeclaration a = DefaultDeclaration { defaultDeclarationContent :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultDeclaration where liftEq = genericLiftEq
instance Ord1 DefaultDeclaration where liftCompare = genericLiftCompare
instance Show1 DefaultDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultDeclaration

data EqualityConstraint a = EqualityConstraint { equalityConstraintLeft :: a, equalityConstraintRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 EqualityConstraint where liftEq = genericLiftEq
instance Ord1 EqualityConstraint where liftCompare = genericLiftCompare
instance Show1 EqualityConstraint where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EqualityConstraint

data EntityIdentifier a = TypeVariableIdentifier Name
                        | TypeConstructorIdentifier Name
                        | ModuleIdentifier Name
                        | ConstructorIdentifier Name
                        | InfixVariableIdentifier Name
                        | TypeClassIdentifier Name
                        | VariableIdentifier Name
                        | PrimitiveConstructorIdentifier Name
                        | PrimitiveVariableIdentifier Name
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 EntityIdentifier where liftEq = genericLiftEq
instance Ord1 EntityIdentifier where liftCompare = genericLiftCompare
instance Show1 EntityIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EntityIdentifier

data Operator a = VariableOperator a
                | ConstructorOperator a
                | TypeOperator Name
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Operator where liftEq = genericLiftEq
instance Ord1 Operator where liftCompare = genericLiftCompare
instance Show1 Operator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Operator

newtype ConstructorSymbol a = ConstructorSymbol { constructorSymbolName :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ConstructorSymbol where liftEq = genericLiftEq
instance Ord1 ConstructorSymbol where liftCompare = genericLiftCompare
instance Show1 ConstructorSymbol where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorSymbol

newtype VariableSymbol a = VariableSymbol { variableSymbolName :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 VariableSymbol where liftEq = genericLiftEq
instance Ord1 VariableSymbol where liftCompare = genericLiftCompare
instance Show1 VariableSymbol where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableSymbol

data StandaloneDerivingInstance a = StandaloneDerivingInstance { standaloneDerivingInstanceContext :: [a], standaloneDerivingInstanceClass :: a, standaloneDerivingInstanceInstance :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 StandaloneDerivingInstance where liftEq = genericLiftEq
instance Ord1 StandaloneDerivingInstance where liftCompare = genericLiftCompare
instance Show1 StandaloneDerivingInstance where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StandaloneDerivingInstance

data ImportDeclaration a = ImportDeclaration { importPackageQualifiedContent :: a, importModule :: a, importSpec :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportDeclaration where liftEq = genericLiftEq
instance Ord1 ImportDeclaration where liftCompare = genericLiftCompare
instance Show1 ImportDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImportDeclaration

data QualifiedImportDeclaration a = QualifiedImportDeclaration { qualifiedImportPackageQualifiedContent :: a, qualifiedImportModule :: a, qualifiedImportSpec :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedImportDeclaration where liftEq = genericLiftEq
instance Ord1 QualifiedImportDeclaration where liftCompare = genericLiftCompare
instance Show1 QualifiedImportDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedImportDeclaration

newtype Import a = Import { importContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Import

newtype HiddenImport a = HiddenImport { hiddenimportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 HiddenImport where liftEq = genericLiftEq
instance Ord1 HiddenImport where liftCompare = genericLiftCompare
instance Show1 HiddenImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable HiddenImport

data ImportAlias a = ImportAlias { importAliasSource :: a, importAliasName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImportAlias

data App a = App { appLeft :: a, appRight :: a }
           | InfixOperatorApp { appLeft :: a, infixOperator :: a, appRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 App where liftEq = genericLiftEq
instance Ord1 App where liftCompare = genericLiftCompare
instance Show1 App where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable App

data ListComprehension a = ListComprehension { comprehensionValue :: a, comprehensionSource :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ListComprehension where liftEq = genericLiftEq
instance Ord1 ListComprehension where liftCompare = genericLiftCompare
instance Show1 ListComprehension where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListComprehension

data Generator a = Generator { generatorValue :: a, generatorSource :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Generator where liftEq = genericLiftEq
instance Ord1 Generator where liftCompare = genericLiftCompare
instance Show1 Generator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Generator

newtype Tuple a = Tuple [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Tuple

newtype TuplePattern a = TuplePattern [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TuplePattern where liftEq = genericLiftEq
instance Ord1 TuplePattern where liftCompare = genericLiftCompare
instance Show1 TuplePattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TuplePattern

data ArithmeticSequence a = EnumFrom a            -- e.g. [1..]
                          | EnumFromThen a a      -- e.g. [1,2..]
                          | EnumFromTo a a        -- e.g. [1..2]
                          | EnumFromThenTo a a a  -- e.g. [1,2..10]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ArithmeticSequence where liftEq = genericLiftEq
instance Ord1 ArithmeticSequence where liftCompare = genericLiftCompare
instance Show1 ArithmeticSequence where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ArithmeticSequence

data OperatorSection a = RightOperatorSection a a
                       | LeftOperatorSection a a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 OperatorSection where liftEq = genericLiftEq
instance Ord1 OperatorSection where liftCompare = genericLiftCompare
instance Show1 OperatorSection where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable OperatorSection

newtype ConstructorPattern a = ConstructorPattern a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ConstructorPattern where liftEq = genericLiftEq
instance Ord1 ConstructorPattern where liftCompare = genericLiftCompare
instance Show1 ConstructorPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorPattern

-- e.g. `a <- b` in a Haskell do block.
data BindPattern a = BindPattern { bindPatternLeft :: a, bindPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 BindPattern where liftEq = genericLiftEq
instance Ord1 BindPattern where liftCompare = genericLiftCompare
instance Show1 BindPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable BindPattern

newtype Do a = Do [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Do where liftEq = genericLiftEq
instance Ord1 Do where liftCompare = genericLiftCompare
instance Show1 Do where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Do

data Lambda a = Lambda { lambdaHead :: a, lambdaBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Lambda where liftEq = genericLiftEq
instance Ord1 Lambda where liftCompare = genericLiftCompare
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Lambda

-- e.g. -1 or (-a) as an expression and not `-` as a variable operator.
newtype PrefixNegation a = PrefixNegation a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PrefixNegation where liftEq = genericLiftEq
instance Ord1 PrefixNegation where liftCompare = genericLiftCompare
instance Show1 PrefixNegation where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PrefixNegation

newtype CPPDirective a = CPPDirective Name
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 CPPDirective where liftEq = genericLiftEq
instance Ord1 CPPDirective where liftCompare = genericLiftCompare
instance Show1 CPPDirective where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable CPPDirective

data QuasiQuotation a = QuasiQuotation { quasiQuotationHead :: a, quasiQuotationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotation where liftEq = genericLiftEq
instance Ord1 QuasiQuotation where liftCompare = genericLiftCompare
instance Show1 QuasiQuotation where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotation

newtype QuasiQuotationExpressionBody a = QuasiQuotationExpressionBody Name
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationExpressionBody where liftEq = genericLiftEq
instance Ord1 QuasiQuotationExpressionBody where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationExpressionBody where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationExpressionBody

data QuasiQuotationPattern a = QuasiQuotationPattern
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationPattern where liftEq = genericLiftEq
instance Ord1 QuasiQuotationPattern where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationPattern

data QuasiQuotationType a = QuasiQuotationType
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationType where liftEq = genericLiftEq
instance Ord1 QuasiQuotationType where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationType

data QuasiQuotationDeclaration a = QuasiQuotationDeclaration
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationDeclaration where liftEq = genericLiftEq
instance Ord1 QuasiQuotationDeclaration where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationDeclaration

newtype QuasiQuotationQuoter a = QuasiQuotationQuoter Name
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationQuoter where liftEq = genericLiftEq
instance Ord1 QuasiQuotationQuoter where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationQuoter where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationQuoter

data QuasiQuotationExpression a = QuasiQuotationExpression
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QuasiQuotationExpression where liftEq = genericLiftEq
instance Ord1 QuasiQuotationExpression where liftCompare = genericLiftCompare
instance Show1 QuasiQuotationExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuasiQuotationExpression

newtype Splice a = Splice a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Splice where liftEq = genericLiftEq
instance Ord1 Splice where liftCompare = genericLiftCompare
instance Show1 Splice where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Splice

data TypeClass a = TypeClass { typeClassContext :: a, typeClassIdentifier :: a, typeClassParameters :: [a], typeClassBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeClass where liftEq = genericLiftEq
instance Ord1 TypeClass where liftCompare = genericLiftCompare
instance Show1 TypeClass where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeClass

data Fixity' a = Fixity' { fixityPrecedence :: a, fixityIdentifier :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Fixity' where liftEq = genericLiftEq
instance Ord1 Fixity' where liftCompare = genericLiftCompare
instance Show1 Fixity' where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Fixity'

-- The default signature of a type class. The default signature has the same shape as a TypeSignature Assignment.
data DefaultSignature a = DefaultSignature { defaultSignatureName :: [a], defaultSignatureContext :: [a], defaultSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultSignature where liftEq = genericLiftEq
instance Ord1 DefaultSignature where liftCompare = genericLiftCompare
instance Show1 DefaultSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultSignature

data TypeFamily a = TypeFamily { typeFamilyIdentifier :: a, typeFamilyParameters :: [a], typeFamilySignature :: a, typeFamilyBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeFamily where liftEq = genericLiftEq
instance Ord1 TypeFamily where liftCompare = genericLiftCompare
instance Show1 TypeFamily where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeFamily

newtype FunctionalDependency a = FunctionalDependency { functionalDependencyContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FunctionalDependency where liftEq = genericLiftEq
instance Ord1 FunctionalDependency where liftCompare = genericLiftCompare
instance Show1 FunctionalDependency where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionalDependency
