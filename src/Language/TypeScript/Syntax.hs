{-# LANGUAGE DeriveAnyClass #-}
module Language.TypeScript.Syntax where

import Prologue
import Data.Abstract.Evaluatable
import Diffing.Algorithm

-- | Lookup type for a type-level key in a typescript map.
data LookupType a = LookupType { lookupTypeIdentifier :: a, lookupTypeKey :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 LookupType where liftEq = genericLiftEq
instance Ord1 LookupType where liftCompare = genericLiftCompare
instance Show1 LookupType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LookupType

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Ord1 ShorthandPropertyIdentifier where liftCompare = genericLiftCompare
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ShorthandPropertyIdentifier

data Union a = Union { _unionLeft :: !a, _unionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Language.TypeScript.Syntax.Union where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Union where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Union where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.Union

data Intersection a = Intersection { _intersectionLeft :: !a, _intersectionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Ord1 Intersection where liftCompare = genericLiftCompare
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Intersection

data FunctionType a = FunctionType { _functionTypeParameters :: !a, _functionFormalParameters :: ![a], _functionType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FunctionType

data AmbientFunction a = AmbientFunction { _ambientFunctionContext :: ![a], _ambientFunctionIdentifier :: !a, _ambientFunctionParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Ord1 AmbientFunction where liftCompare = genericLiftCompare
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AmbientFunction

data ImportRequireClause a = ImportRequireClause { _importRequireIdentifier :: !a, _importRequireSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportRequireClause where liftEq = genericLiftEq
instance Ord1 ImportRequireClause where liftCompare = genericLiftCompare
instance Show1 ImportRequireClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportRequireClause

newtype ImportClause a = ImportClause { _importClauseElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportClause where liftEq = genericLiftEq
instance Ord1 ImportClause where liftCompare = genericLiftCompare
instance Show1 ImportClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportClause

newtype NamespaceImport a = NamespaceImport { _namespaceImportSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 NamespaceImport where liftEq = genericLiftEq
instance Ord1 NamespaceImport where liftCompare = genericLiftCompare
instance Show1 NamespaceImport where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NamespaceImport

newtype Tuple a = Tuple { _tupleElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- This is a tuple type, not a tuple value, so we can't lean on the shared Tuple value
instance Evaluatable Tuple

data Constructor a = Constructor { _constructorTypeParameters :: !a, _constructorFormalParameters :: ![a], _constructorType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Language.TypeScript.Syntax.Constructor where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Constructor where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Constructor where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.Constructor

data TypeParameter a = TypeParameter { _typeParameter :: !a, _typeParameterConstraint :: !a, _typeParameterDefaultType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Ord1 TypeParameter where liftCompare = genericLiftCompare
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeParameter

data TypeAssertion a = TypeAssertion { _typeAssertionParameters :: !a, _typeAssertionExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeAssertion where liftEq = genericLiftEq
instance Ord1 TypeAssertion where liftCompare = genericLiftCompare
instance Show1 TypeAssertion where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeAssertion

newtype Annotation a = Annotation { _annotationType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Annotation

newtype Decorator a = Decorator { _decoratorTerm :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Decorator

newtype ComputedPropertyName a = ComputedPropertyName a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ComputedPropertyName where liftEq = genericLiftEq
instance Ord1 ComputedPropertyName where liftCompare = genericLiftCompare
instance Show1 ComputedPropertyName where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ComputedPropertyName

newtype Constraint a = Constraint { _constraintType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Ord1 Constraint where liftCompare = genericLiftCompare
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Constraint

newtype DefaultType a = DefaultType { _defaultType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 DefaultType where liftEq = genericLiftEq
instance Ord1 DefaultType where liftCompare = genericLiftCompare
instance Show1 DefaultType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable DefaultType

newtype ParenthesizedType a = ParenthesizedType { _parenthesizedType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Ord1 ParenthesizedType where liftCompare = genericLiftCompare
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ParenthesizedType

newtype PredefinedType a = PredefinedType { _predefinedType :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Ord1 PredefinedType where liftCompare = genericLiftCompare
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable PredefinedType

newtype TypeIdentifier a = TypeIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Ord1 TypeIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeIdentifier

data NestedIdentifier a = NestedIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Ord1 NestedIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedIdentifier

data NestedTypeIdentifier a = NestedTypeIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 NestedTypeIdentifier where liftEq = genericLiftEq
instance Ord1 NestedTypeIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedTypeIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedTypeIdentifier

data GenericType a = GenericType { _genericTypeIdentifier :: !a, _genericTypeArguments :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Ord1 GenericType where liftCompare = genericLiftCompare
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable GenericType

data TypePredicate a = TypePredicate { _typePredicateIdentifier :: !a, _typePredicateType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypePredicate where liftEq = genericLiftEq
instance Ord1 TypePredicate where liftCompare = genericLiftCompare
instance Show1 TypePredicate where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypePredicate

newtype ObjectType a = ObjectType { _objectTypeElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ObjectType where liftEq = genericLiftEq
instance Ord1 ObjectType where liftCompare = genericLiftCompare
instance Show1 ObjectType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ObjectType

newtype Export a = Export { _exportElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Export where liftEq = genericLiftEq
instance Ord1 Export where liftCompare = genericLiftCompare
instance Show1 Export where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Export

newtype ExportClause a = ExportClause { _exportClauseElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ExportClause where liftEq = genericLiftEq
instance Ord1 ExportClause where liftCompare = genericLiftCompare
instance Show1 ExportClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExportClause

data ImportExportSpecifier a = ImportExportSpecifier { _specifierSubject :: !a, _specifierAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportExportSpecifier where liftEq = genericLiftEq
instance Ord1 ImportExportSpecifier where liftCompare = genericLiftCompare
instance Show1 ImportExportSpecifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportExportSpecifier

newtype NamedImports a = NamedImports { _namedImportElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 NamedImports where liftEq = genericLiftEq
instance Ord1 NamedImports where liftCompare = genericLiftCompare
instance Show1 NamedImports where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NamedImports

data With a = With { _withExpression :: !a, _withBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 With where liftEq = genericLiftEq
instance Ord1 With where liftCompare = genericLiftCompare
instance Show1 With where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable With

newtype AmbientDeclaration a = AmbientDeclaration { _ambientDeclarationBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Ord1 AmbientDeclaration where liftCompare = genericLiftCompare
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AmbientDeclaration

data EnumDeclaration a = EnumDeclaration { _enumDeclarationIdentifier :: !a, _enumDeclarationBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable EnumDeclaration

newtype ExtendsClause a = ExtendsClause { _extendsClauses :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Ord1 ExtendsClause where liftCompare = genericLiftCompare
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExtendsClause

newtype ArrayType a = ArrayType { _arrayType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ArrayType

newtype FlowMaybeType a = FlowMaybeType { _flowMaybeType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 FlowMaybeType where liftEq = genericLiftEq
instance Ord1 FlowMaybeType where liftCompare = genericLiftCompare
instance Show1 FlowMaybeType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FlowMaybeType

newtype TypeQuery a = TypeQuery { _typeQuerySubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeQuery where liftEq = genericLiftEq
instance Ord1 TypeQuery where liftCompare = genericLiftCompare
instance Show1 TypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeQuery

newtype IndexTypeQuery a = IndexTypeQuery { _indexTypeQuerySubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 IndexTypeQuery where liftEq = genericLiftEq
instance Ord1 IndexTypeQuery where liftCompare = genericLiftCompare
instance Show1 IndexTypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexTypeQuery

newtype TypeArguments a = TypeArguments { _typeArguments :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeArguments where liftEq = genericLiftEq
instance Ord1 TypeArguments where liftCompare = genericLiftCompare
instance Show1 TypeArguments where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeArguments

newtype ThisType a = ThisType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ThisType where liftEq = genericLiftEq
instance Ord1 ThisType where liftCompare = genericLiftCompare
instance Show1 ThisType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ThisType

newtype ExistentialType a = ExistentialType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ExistentialType where liftEq = genericLiftEq
instance Ord1 ExistentialType where liftCompare = genericLiftCompare
instance Show1 ExistentialType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExistentialType

newtype LiteralType a = LiteralType { _literalTypeSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 LiteralType where liftEq = genericLiftEq
instance Ord1 LiteralType where liftCompare = genericLiftCompare
instance Show1 LiteralType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LiteralType

data PropertySignature a = PropertySignature { _modifiers :: ![a], _propertySignaturePropertyName :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Ord1 PropertySignature where liftCompare = genericLiftCompare
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable PropertySignature

data CallSignature a = CallSignature { _callSignatureTypeParameters :: !a, _callSignatureParameters :: ![a], _callSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Ord1 CallSignature where liftCompare = genericLiftCompare
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable CallSignature

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { _constructSignatureTypeParameters :: !a, _constructSignatureParameters :: ![a], _constructSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Ord1 ConstructSignature where liftCompare = genericLiftCompare
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ConstructSignature

data IndexSignature a = IndexSignature { _indexSignatureSubject :: a, _indexSignatureType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Ord1 IndexSignature where liftCompare = genericLiftCompare
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexSignature

data AbstractMethodSignature a = AbstractMethodSignature { _abstractMethodSignatureContext :: ![a], _abstractMethodSignatureName :: !a, _abstractMethodSignatureParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 AbstractMethodSignature where liftEq = genericLiftEq
instance Ord1 AbstractMethodSignature where liftCompare = genericLiftCompare
instance Show1 AbstractMethodSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AbstractMethodSignature

data Debugger a = Debugger
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Ord1 Debugger where liftCompare = genericLiftCompare
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Debugger

data ForOf a = ForOf { _forOfBinding :: !a, _forOfSubject :: !a, _forOfBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Ord1 ForOf where liftCompare = genericLiftCompare
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ForOf

data This a = This
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable This

data LabeledStatement a = LabeledStatement { _labeledStatementIdentifier :: !a, _labeledStatementSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LabeledStatement

newtype Update a = Update { _updateSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Update

data ImportAlias a = ImportAlias { _importAliasSubject :: !a, _importAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportAlias

data InternalModule a = InternalModule { _internalModuleIdentifier :: !a, _internalModuleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Ord1 InternalModule where liftCompare = genericLiftCompare
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable InternalModule

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Super

data Undefined a = Undefined
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Ord1 Undefined where liftCompare = genericLiftCompare
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Undefined

data ClassHeritage a = ClassHeritage { _classHeritageExtendsClause :: !a, _implementsClause :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Ord1 ClassHeritage where liftCompare = genericLiftCompare
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ClassHeritage

data AbstractClass a = AbstractClass { _abstractClassIdentifier :: !a,  _abstractClassTypeParameters :: !a, _classHeritage :: ![a], _classBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Ord1 AbstractClass where liftCompare = genericLiftCompare
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AbstractClass

data JsxElement a = JsxElement { _jsxOpeningElement :: !a,  _jsxElements :: ![a], _jsxClosingElement :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxElement where liftEq = genericLiftEq
instance Ord1 JsxElement where liftCompare = genericLiftCompare
instance Show1 JsxElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxElement

newtype JsxText a = JsxText ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxText where liftEq = genericLiftEq
instance Ord1 JsxText where liftCompare = genericLiftCompare
instance Show1 JsxText where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxText

newtype JsxExpression a = JsxExpression { _jsxExpression :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxExpression where liftEq = genericLiftEq
instance Ord1 JsxExpression where liftCompare = genericLiftCompare
instance Show1 JsxExpression where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxExpression

data JsxOpeningElement a = JsxOpeningElement { _jsxOpeningElementIdentifier :: !a,  _jsxAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxOpeningElement where liftEq = genericLiftEq
instance Ord1 JsxOpeningElement where liftCompare = genericLiftCompare
instance Show1 JsxOpeningElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxOpeningElement

newtype JsxClosingElement a = JsxClosingElement { _jsxClosingElementIdentifier :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxClosingElement where liftEq = genericLiftEq
instance Ord1 JsxClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxClosingElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxClosingElement

data JsxSelfClosingElement a = JsxSelfClosingElement { _jsxSelfClosingElementIdentifier :: !a, _jsxSelfClosingElementAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxSelfClosingElement where liftEq = genericLiftEq
instance Ord1 JsxSelfClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxSelfClosingElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxSelfClosingElement

data JsxAttribute a = JsxAttribute { _jsxAttributeTarget :: !a, _jsxAttributeValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxAttribute where liftEq = genericLiftEq
instance Ord1 JsxAttribute where liftCompare = genericLiftCompare
instance Show1 JsxAttribute where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxAttribute

newtype ImplementsClause a = ImplementsClause { _implementsClauseTypes :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Ord1 ImplementsClause where liftCompare = genericLiftCompare
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImplementsClause

data OptionalParameter a = OptionalParameter { _optionalParameterContext :: ![a], _optionalParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable OptionalParameter

data RequiredParameter a = RequiredParameter { _requiredParameterContext :: ![a], _requiredParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RequiredParameter

data RestParameter a = RestParameter { _restParameterContext :: ![a], _restParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 RestParameter where liftEq = genericLiftEq
instance Ord1 RestParameter where liftCompare = genericLiftCompare
instance Show1 RestParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RestParameter

newtype JsxFragment a = JsxFragment [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxFragment where liftEq = genericLiftEq
instance Ord1 JsxFragment where liftCompare = genericLiftCompare
instance Show1 JsxFragment where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxFragment

data JsxNamespaceName a = JsxNamespaceName a a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 JsxNamespaceName where liftEq = genericLiftEq
instance Ord1 JsxNamespaceName where liftCompare = genericLiftCompare
instance Show1 JsxNamespaceName where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxNamespaceName
