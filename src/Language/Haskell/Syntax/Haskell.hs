{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Haskell where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data Module a = Module { moduleContext    :: [a]
                       , moduleIdentifier :: a
                       , moduleExports    :: [a]
                       , moduleStatements :: a
                       }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Module

instance Evaluatable Module

data Field a = Field { fieldName :: !a, fieldBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Field

instance Evaluatable Field

newtype Pragma a = Pragma { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Pragma

instance Evaluatable Pragma

newtype Deriving a = Deriving { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Deriving

instance Evaluatable Deriving

newtype ContextAlt a = ContextAlt { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ContextAlt

instance Evaluatable ContextAlt

newtype Class a = Class { classContent :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Class

instance Evaluatable Class

data GADT a = GADT { gadtContext :: a, gadtName :: a, gadtConstructors :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically GADT

instance Evaluatable GADT

newtype Export a = Export { exportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Export

instance Evaluatable Export

newtype ModuleExport a = ModuleExport { moduleExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ModuleExport

instance Evaluatable ModuleExport

data InfixOperatorPattern a = InfixOperatorPattern { infixOperatorPatternLeft :: a, infixOperatorPatternOperator :: a, infixOperatorPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InfixOperatorPattern

instance Evaluatable InfixOperatorPattern

newtype QuotedName a = QuotedName { quotedNameContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QuotedName

instance Evaluatable QuotedName

newtype ScopedTypeVariables a = ScopedTypeVariables { scopedTypeVariablesContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ScopedTypeVariables

instance Evaluatable ScopedTypeVariables

data NewType a = NewType { newTypeContext :: [a], newTypeLeft :: a, newTypeRight :: a, newTypeDeriving :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NewType

instance Evaluatable NewType

newtype DefaultDeclaration a = DefaultDeclaration { defaultDeclarationContent :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DefaultDeclaration

instance Evaluatable DefaultDeclaration


newtype VariableOperator a = VariableOperator { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 VariableOperator where liftEq = genericLiftEq
instance Ord1 VariableOperator where liftCompare = genericLiftCompare
instance Show1 VariableOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableOperator

newtype ConstructorOperator a = ConstructorOperator { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ConstructorOperator where liftEq = genericLiftEq
instance Ord1 ConstructorOperator where liftCompare = genericLiftCompare
instance Show1 ConstructorOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorOperator

newtype TypeOperator a = TypeOperator { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeOperator where liftEq = genericLiftEq
instance Ord1 TypeOperator where liftCompare = genericLiftCompare
instance Show1 TypeOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeOperator

newtype PromotedTypeOperator a = PromotedTypeOperator { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PromotedTypeOperator where liftEq = genericLiftEq
instance Ord1 PromotedTypeOperator where liftCompare = genericLiftCompare
instance Show1 PromotedTypeOperator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PromotedTypeOperator

newtype VariableSymbol a = VariableSymbol { variableSymbolName :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 VariableSymbol where liftEq = genericLiftEq
instance Ord1 VariableSymbol where liftCompare = genericLiftCompare
instance Show1 VariableSymbol where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableSymbol

data ImportDeclaration a = ImportDeclaration { importPackageQualifiedContent :: a, importModule :: a, importSpec :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ImportDeclaration

instance Evaluatable ImportDeclaration

data QualifiedImportDeclaration a = QualifiedImportDeclaration { qualifiedImportPackageQualifiedContent :: a, qualifiedImportModule :: a, qualifiedImportSpec :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedImportDeclaration

instance Evaluatable QualifiedImportDeclaration

newtype Import a = Import { importContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Import

instance Evaluatable Import

newtype HiddenImport a = HiddenImport { hiddenimportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically HiddenImport

instance Evaluatable HiddenImport

data ImportAlias a = ImportAlias { importAliasSource :: a, importAliasName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ImportAlias

instance Evaluatable ImportAlias

data App a = App { appLeft :: a, appLeftTypeApp :: a, appRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically App

instance Evaluatable App

data InfixOperatorApp a = InfixOperatorApp { infixOperatorAppLeft :: a, infixOperatorAppLeftTypeApp :: a, infixOperatorAppOperator :: a, infixOperatorAppRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InfixOperatorApp

instance Evaluatable InfixOperatorApp

newtype TypeApp a = TypeApp { typeAppType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeApp

instance Evaluatable TypeApp

data ListComprehension a = ListComprehension { comprehensionValue :: a, comprehensionSource :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ListComprehension

instance Evaluatable ListComprehension

data Generator a = Generator { generatorValue :: a, generatorSource :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Generator

instance Evaluatable Generator

newtype TupleExpression a = TupleExpression { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TupleExpression

instance Evaluatable TupleExpression

newtype TuplePattern a = TuplePattern { value :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TuplePattern

instance Evaluatable TuplePattern

-- e.g. [1..], [1,2..], [1,2..10]
data ArithmeticSequence a = ArithmeticSequence { from :: a, next :: Maybe a, to :: Maybe a  }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ArithmeticSequence

instance Evaluatable ArithmeticSequence

data RightOperatorSection a = RightOperatorSection { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RightOperatorSection

instance Evaluatable RightOperatorSection

data LeftOperatorSection a = LeftOperatorSection { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LeftOperatorSection

instance Evaluatable LeftOperatorSection

newtype ConstructorPattern a = ConstructorPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ConstructorPattern

instance Evaluatable ConstructorPattern

-- e.g. `a <- b` in a Haskell do block.
data BindPattern a = BindPattern { bindPatternLeft :: [a], bindPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BindPattern

instance Evaluatable BindPattern

newtype Do a = Do { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Do

instance Evaluatable Do

data Lambda a = Lambda { lambdaHead :: a, lambdaBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Lambda

instance Evaluatable Lambda

-- e.g. -1 or (-a) as an expression and not `-` as a variable operator.
newtype PrefixNegation a = PrefixNegation { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PrefixNegation

instance Evaluatable PrefixNegation

newtype CPPDirective a = CPPDirective { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically CPPDirective

instance Evaluatable CPPDirective

data FixityAlt a = FixityAlt { fixityPrecedence :: a, fixityIdentifier :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FixityAlt

instance Evaluatable FixityAlt

-- e.g. The `{..}` in `foo Bar{..} = baz`
data RecordWildCards a = RecordWildCards
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RecordWildCards

instance Evaluatable RecordWildCards

data Wildcard a = Wildcard
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Wildcard

instance Evaluatable Wildcard

data Let a = Let { letStatements :: [a], letInClause :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Let

instance Evaluatable Let

-- e.g. The `start` or `end` in `f Blob{start, end} = [start, end]`.
newtype NamedFieldPun a = NamedFieldPun { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamedFieldPun

instance Evaluatable NamedFieldPun

-- e.g. The `-(1)` in `f (-(1)) = 1`.
newtype NegativeLiteral a = NegativeLiteral { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NegativeLiteral

instance Evaluatable NegativeLiteral

newtype LambdaCase a = LambdaCase { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LambdaCase

instance Evaluatable LambdaCase

-- The `y { a = 1, b = 2} in `f y@Example = y { a = 1, b = 2 }`.
newtype LabeledUpdate a = LabeledUpdate { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LabeledUpdate

instance Evaluatable LabeledUpdate

-- The `a = 1` in `f y@Example = y { a = 1, b = 2 }`.
data FieldBind a = FieldBind { fieldBindLeft :: a, fieldBindRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FieldBind

instance Evaluatable FieldBind
