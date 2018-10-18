{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
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

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module

data Field a = Field { fieldName :: !a, fieldBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Field where liftEq = genericLiftEq
instance Ord1 Field where liftCompare = genericLiftCompare
instance Show1 Field where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Field

newtype Pragma a = Pragma { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Pragma where liftEq = genericLiftEq
instance Ord1 Pragma where liftCompare = genericLiftCompare
instance Show1 Pragma where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Pragma

newtype Deriving a = Deriving { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Deriving where liftEq = genericLiftEq
instance Ord1 Deriving where liftCompare = genericLiftCompare
instance Show1 Deriving where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Deriving

newtype ContextAlt a = ContextAlt { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ContextAlt where liftEq = genericLiftEq
instance Ord1 ContextAlt where liftCompare = genericLiftCompare
instance Show1 ContextAlt where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ContextAlt

newtype Class a = Class { classContent :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class

data GADT a = GADT { gadtContext :: a, gadtName :: a, gadtConstructors :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 GADT where liftEq = genericLiftEq
instance Ord1 GADT where liftCompare = genericLiftCompare
instance Show1 GADT where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GADT

newtype Export a = Export { exportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Export where liftEq = genericLiftEq
instance Ord1 Export where liftCompare = genericLiftCompare
instance Show1 Export where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Export

newtype ModuleExport a = ModuleExport { moduleExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ModuleExport where liftEq = genericLiftEq
instance Ord1 ModuleExport where liftCompare = genericLiftCompare
instance Show1 ModuleExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ModuleExport

data InfixOperatorPattern a = InfixOperatorPattern { infixOperatorPatternLeft :: a, infixOperatorPatternOperator :: a, infixOperatorPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InfixOperatorPattern where liftEq = genericLiftEq
instance Ord1 InfixOperatorPattern where liftCompare = genericLiftCompare
instance Show1 InfixOperatorPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixOperatorPattern

newtype QuotedName a = QuotedName { quotedNameContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QuotedName where liftEq = genericLiftEq
instance Ord1 QuotedName where liftCompare = genericLiftCompare
instance Show1 QuotedName where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QuotedName

newtype ScopedTypeVariables a = ScopedTypeVariables { scopedTypeVariablesContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ScopedTypeVariables where liftEq = genericLiftEq
instance Ord1 ScopedTypeVariables where liftCompare = genericLiftCompare
instance Show1 ScopedTypeVariables where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ScopedTypeVariables

data NewType a = NewType { newTypeContext :: [a], newTypeLeft :: a, newTypeRight :: a, newTypeDeriving :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 NewType where liftEq = genericLiftEq
instance Ord1 NewType where liftCompare = genericLiftCompare
instance Show1 NewType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NewType

newtype DefaultDeclaration a = DefaultDeclaration { defaultDeclarationContent :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 DefaultDeclaration where liftEq = genericLiftEq
instance Ord1 DefaultDeclaration where liftCompare = genericLiftCompare
instance Show1 DefaultDeclaration where liftShowsPrec = genericLiftShowsPrec

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

instance Eq1 ImportDeclaration where liftEq = genericLiftEq
instance Ord1 ImportDeclaration where liftCompare = genericLiftCompare
instance Show1 ImportDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImportDeclaration

data QualifiedImportDeclaration a = QualifiedImportDeclaration { qualifiedImportPackageQualifiedContent :: a, qualifiedImportModule :: a, qualifiedImportSpec :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedImportDeclaration where liftEq = genericLiftEq
instance Ord1 QualifiedImportDeclaration where liftCompare = genericLiftCompare
instance Show1 QualifiedImportDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedImportDeclaration

newtype Import a = Import { importContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Import

newtype HiddenImport a = HiddenImport { hiddenimportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 HiddenImport where liftEq = genericLiftEq
instance Ord1 HiddenImport where liftCompare = genericLiftCompare
instance Show1 HiddenImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable HiddenImport

data ImportAlias a = ImportAlias { importAliasSource :: a, importAliasName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImportAlias

data App a = App { appLeft :: a, appLeftTypeApp :: a, appRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 App where liftEq = genericLiftEq
instance Ord1 App where liftCompare = genericLiftCompare
instance Show1 App where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable App

data InfixOperatorApp a = InfixOperatorApp { infixOperatorAppLeft :: a, infixOperatorAppLeftTypeApp :: a, infixOperatorAppOperator :: a, infixOperatorAppRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InfixOperatorApp where liftEq = genericLiftEq
instance Ord1 InfixOperatorApp where liftCompare = genericLiftCompare
instance Show1 InfixOperatorApp where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixOperatorApp

newtype TypeApp a = TypeApp { typeAppType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeApp where liftEq = genericLiftEq
instance Ord1 TypeApp where liftCompare = genericLiftCompare
instance Show1 TypeApp where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeApp

data ListComprehension a = ListComprehension { comprehensionValue :: a, comprehensionSource :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ListComprehension where liftEq = genericLiftEq
instance Ord1 ListComprehension where liftCompare = genericLiftCompare
instance Show1 ListComprehension where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListComprehension

data Generator a = Generator { generatorValue :: a, generatorSource :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Generator where liftEq = genericLiftEq
instance Ord1 Generator where liftCompare = genericLiftCompare
instance Show1 Generator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Generator

newtype TupleExpression a = TupleExpression { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TupleExpression where liftEq = genericLiftEq
instance Ord1 TupleExpression where liftCompare = genericLiftCompare
instance Show1 TupleExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TupleExpression

newtype TuplePattern a = TuplePattern { value :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TuplePattern where liftEq = genericLiftEq
instance Ord1 TuplePattern where liftCompare = genericLiftCompare
instance Show1 TuplePattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TuplePattern

-- e.g. [1..], [1,2..], [1,2..10]
data ArithmeticSequence a = ArithmeticSequence { from :: a, next :: Maybe a, to :: Maybe a  }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ArithmeticSequence where liftEq = genericLiftEq
instance Ord1 ArithmeticSequence where liftCompare = genericLiftCompare
instance Show1 ArithmeticSequence where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ArithmeticSequence

data RightOperatorSection a = RightOperatorSection { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 RightOperatorSection where liftEq = genericLiftEq
instance Ord1 RightOperatorSection where liftCompare = genericLiftCompare
instance Show1 RightOperatorSection where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RightOperatorSection

data LeftOperatorSection a = LeftOperatorSection { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LeftOperatorSection where liftEq = genericLiftEq
instance Ord1 LeftOperatorSection where liftCompare = genericLiftCompare
instance Show1 LeftOperatorSection where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LeftOperatorSection

newtype ConstructorPattern a = ConstructorPattern { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ConstructorPattern where liftEq = genericLiftEq
instance Ord1 ConstructorPattern where liftCompare = genericLiftCompare
instance Show1 ConstructorPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorPattern

-- e.g. `a <- b` in a Haskell do block.
data BindPattern a = BindPattern { bindPatternLeft :: [a], bindPatternRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 BindPattern where liftEq = genericLiftEq
instance Ord1 BindPattern where liftCompare = genericLiftCompare
instance Show1 BindPattern where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable BindPattern

newtype Do a = Do { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Do where liftEq = genericLiftEq
instance Ord1 Do where liftCompare = genericLiftCompare
instance Show1 Do where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Do

data Lambda a = Lambda { lambdaHead :: a, lambdaBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Lambda where liftEq = genericLiftEq
instance Ord1 Lambda where liftCompare = genericLiftCompare
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Lambda

-- e.g. -1 or (-a) as an expression and not `-` as a variable operator.
newtype PrefixNegation a = PrefixNegation { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PrefixNegation where liftEq = genericLiftEq
instance Ord1 PrefixNegation where liftCompare = genericLiftCompare
instance Show1 PrefixNegation where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PrefixNegation

newtype CPPDirective a = CPPDirective { value :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 CPPDirective where liftEq = genericLiftEq
instance Ord1 CPPDirective where liftCompare = genericLiftCompare
instance Show1 CPPDirective where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable CPPDirective

data FixityAlt a = FixityAlt { fixityPrecedence :: a, fixityIdentifier :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FixityAlt where liftEq = genericLiftEq
instance Ord1 FixityAlt where liftCompare = genericLiftCompare
instance Show1 FixityAlt where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FixityAlt

-- e.g. The `{..}` in `foo Bar{..} = baz`
data RecordWildCards a = RecordWildCards
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 RecordWildCards where liftEq = genericLiftEq
instance Ord1 RecordWildCards where liftCompare = genericLiftCompare
instance Show1 RecordWildCards where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RecordWildCards

data Wildcard a = Wildcard
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Wildcard where liftEq = genericLiftEq
instance Ord1 Wildcard where liftCompare = genericLiftCompare
instance Show1 Wildcard where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Wildcard

data Let a = Let { letStatements :: [a], letInClause :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Let where liftEq = genericLiftEq
instance Ord1 Let where liftCompare = genericLiftCompare
instance Show1 Let where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Let

-- e.g. The `start` or `end` in `f Blob{start, end} = [start, end]`.
newtype NamedFieldPun a = NamedFieldPun { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 NamedFieldPun where liftEq = genericLiftEq
instance Ord1 NamedFieldPun where liftCompare = genericLiftCompare
instance Show1 NamedFieldPun where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NamedFieldPun

-- e.g. The `-(1)` in `f (-(1)) = 1`.
newtype NegativeLiteral a = NegativeLiteral { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 NegativeLiteral where liftEq = genericLiftEq
instance Ord1 NegativeLiteral where liftCompare = genericLiftCompare
instance Show1 NegativeLiteral where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NegativeLiteral

newtype LambdaCase a = LambdaCase { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LambdaCase where liftEq = genericLiftEq
instance Ord1 LambdaCase where liftCompare = genericLiftCompare
instance Show1 LambdaCase where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LambdaCase

-- The `y { a = 1, b = 2} in `f y@Example = y { a = 1, b = 2 }`.
newtype LabeledUpdate a = LabeledUpdate { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LabeledUpdate where liftEq = genericLiftEq
instance Ord1 LabeledUpdate where liftCompare = genericLiftCompare
instance Show1 LabeledUpdate where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LabeledUpdate

-- The `a = 1` in `f y@Example = y { a = 1, b = 2 }`.
data FieldBind a = FieldBind { fieldBindLeft :: a, fieldBindRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FieldBind where liftEq = genericLiftEq
instance Ord1 FieldBind where liftCompare = genericLiftCompare
instance Show1 FieldBind where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FieldBind
