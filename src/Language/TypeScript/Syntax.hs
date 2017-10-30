{-# LANGUAGE DeriveAnyClass #-}
module Language.TypeScript.Syntax where

import Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import GHC.Generics

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Ord1 ShorthandPropertyIdentifier where liftCompare = genericLiftCompare
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec

data Union a = Union { _unionLeft :: !a, _unionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Language.TypeScript.Syntax.Union where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Union where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Union where liftShowsPrec = genericLiftShowsPrec

data Intersection a = Intersection { _intersectionLeft :: !a, _intersectionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Ord1 Intersection where liftCompare = genericLiftCompare
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec

data FunctionType a = FunctionType { _functionTypeParameters :: !a, _functionFormalParameters :: ![a], _functionType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec

data AmbientFunction a = AmbientFunction { _ambientFunctionContext :: ![a], _ambientFunctionIdentifier :: !a, _ambientFunctionParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Ord1 AmbientFunction where liftCompare = genericLiftCompare
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec

data ImportRequireClause a = ImportRequireClause { _importRequireIdentifier :: !a, _importRequireSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ImportRequireClause where liftEq = genericLiftEq
instance Ord1 ImportRequireClause where liftCompare = genericLiftCompare
instance Show1 ImportRequireClause where liftShowsPrec = genericLiftShowsPrec

newtype ImportClause a = ImportClause { _importClauseElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ImportClause where liftEq = genericLiftEq
instance Ord1 ImportClause where liftCompare = genericLiftCompare
instance Show1 ImportClause where liftShowsPrec = genericLiftShowsPrec

newtype NamespaceImport a = NamespaceImport { _namespaceImportSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamespaceImport where liftEq = genericLiftEq
instance Ord1 NamespaceImport where liftCompare = genericLiftCompare
instance Show1 NamespaceImport where liftShowsPrec = genericLiftShowsPrec

newtype Tuple a = Tuple { _tupleElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

data Constructor a = Constructor { _constructorTypeParameters :: !a, _constructorFormalParameters :: ![a], _constructorType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Language.TypeScript.Syntax.Constructor where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Constructor where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Constructor where liftShowsPrec = genericLiftShowsPrec

data TypeParameter a = TypeParameter { _typeParameter :: !a, _typeParameterConstraint :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Ord1 TypeParameter where liftCompare = genericLiftCompare
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec

data TypeAssertion a = TypeAssertion { _typeAssertionParameters :: !a, _typeAssertionExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeAssertion where liftEq = genericLiftEq
instance Ord1 TypeAssertion where liftCompare = genericLiftCompare
instance Show1 TypeAssertion where liftShowsPrec = genericLiftShowsPrec

newtype Annotation a = Annotation { _annotationType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

newtype Decorator a = Decorator { _decoratorTerm :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

newtype ComputedPropertyName a = ComputedPropertyName a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ComputedPropertyName where liftEq = genericLiftEq
instance Ord1 ComputedPropertyName where liftCompare = genericLiftCompare
instance Show1 ComputedPropertyName where liftShowsPrec = genericLiftShowsPrec

newtype Constraint a = Constraint { _constraintType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Ord1 Constraint where liftCompare = genericLiftCompare
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec

newtype ParenthesizedType a = ParenthesizedType { _parenthesizedType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Ord1 ParenthesizedType where liftCompare = genericLiftCompare
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec

newtype PredefinedType a = PredefinedType { _predefinedType :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Ord1 PredefinedType where liftCompare = genericLiftCompare
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec

newtype TypeIdentifier a = TypeIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Ord1 TypeIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec

data NestedIdentifier a = NestedIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Ord1 NestedIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec

data NestedTypeIdentifier a = NestedTypeIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NestedTypeIdentifier where liftEq = genericLiftEq
instance Ord1 NestedTypeIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedTypeIdentifier where liftShowsPrec = genericLiftShowsPrec

data GenericType a = GenericType { _genericTypeIdentifier :: !a, _genericTypeArguments :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Ord1 GenericType where liftCompare = genericLiftCompare
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec

data TypePredicate a = TypePredicate { _typePredicateIdentifier :: !a, _typePredicateType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypePredicate where liftEq = genericLiftEq
instance Ord1 TypePredicate where liftCompare = genericLiftCompare
instance Show1 TypePredicate where liftShowsPrec = genericLiftShowsPrec

newtype ObjectType a = ObjectType { _objectTypeElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ObjectType where liftEq = genericLiftEq
instance Ord1 ObjectType where liftCompare = genericLiftCompare
instance Show1 ObjectType where liftShowsPrec = genericLiftShowsPrec

newtype Export a = Export { _exportElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Export where liftEq = genericLiftEq
instance Ord1 Export where liftCompare = genericLiftCompare
instance Show1 Export where liftShowsPrec = genericLiftShowsPrec

newtype ExportClause a = ExportClause { _exportClauseElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ExportClause where liftEq = genericLiftEq
instance Ord1 ExportClause where liftCompare = genericLiftCompare
instance Show1 ExportClause where liftShowsPrec = genericLiftShowsPrec

data ImportExportSpecifier a = ImportExportSpecifier { _specifierSubject :: !a, _specifierAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ImportExportSpecifier where liftEq = genericLiftEq
instance Ord1 ImportExportSpecifier where liftCompare = genericLiftCompare
instance Show1 ImportExportSpecifier where liftShowsPrec = genericLiftShowsPrec

newtype NamedImports a = NamedImports { _namedImportElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 NamedImports where liftEq = genericLiftEq
instance Ord1 NamedImports where liftCompare = genericLiftCompare
instance Show1 NamedImports where liftShowsPrec = genericLiftShowsPrec

data With a = With { _withExpression :: !a, _withBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 With where liftEq = genericLiftEq
instance Ord1 With where liftCompare = genericLiftCompare
instance Show1 With where liftShowsPrec = genericLiftShowsPrec

newtype AmbientDeclaration a = AmbientDeclaration { _ambientDeclarationBody :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Ord1 AmbientDeclaration where liftCompare = genericLiftCompare
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec

data EnumDeclaration a = EnumDeclaration { _enumDeclarationIdentifier :: !a, _enumDeclarationBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec

data ExtendsClause a = ExtendsClause { _extendsSubject :: a, _extendsTypeArguments :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Ord1 ExtendsClause where liftCompare = genericLiftCompare
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec

newtype ArrayType a = ArrayType { _arrayType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec

newtype FlowMaybeType a = FlowMaybeType { _flowMaybeType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 FlowMaybeType where liftEq = genericLiftEq
instance Ord1 FlowMaybeType where liftCompare = genericLiftCompare
instance Show1 FlowMaybeType where liftShowsPrec = genericLiftShowsPrec

newtype TypeQuery a = TypeQuery { _typeQuerySubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeQuery where liftEq = genericLiftEq
instance Ord1 TypeQuery where liftCompare = genericLiftCompare
instance Show1 TypeQuery where liftShowsPrec = genericLiftShowsPrec

newtype IndexTypeQuery a = IndexTypeQuery { _indexTypeQuerySubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 IndexTypeQuery where liftEq = genericLiftEq
instance Ord1 IndexTypeQuery where liftCompare = genericLiftCompare
instance Show1 IndexTypeQuery where liftShowsPrec = genericLiftShowsPrec

newtype TypeArguments a = TypeArguments { _typeArguments :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeArguments where liftEq = genericLiftEq
instance Ord1 TypeArguments where liftCompare = genericLiftCompare
instance Show1 TypeArguments where liftShowsPrec = genericLiftShowsPrec

newtype ThisType a = ThisType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ThisType where liftEq = genericLiftEq
instance Ord1 ThisType where liftCompare = genericLiftCompare
instance Show1 ThisType where liftShowsPrec = genericLiftShowsPrec

newtype ExistentialType a = ExistentialType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ExistentialType where liftEq = genericLiftEq
instance Ord1 ExistentialType where liftCompare = genericLiftCompare
instance Show1 ExistentialType where liftShowsPrec = genericLiftShowsPrec

newtype LiteralType a = LiteralType { _literalTypeSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 LiteralType where liftEq = genericLiftEq
instance Ord1 LiteralType where liftCompare = genericLiftCompare
instance Show1 LiteralType where liftShowsPrec = genericLiftShowsPrec

data PropertySignature a = PropertySignature { _modifiers :: ![a], _propertySignaturePropertyName :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Ord1 PropertySignature where liftCompare = genericLiftCompare
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec

data CallSignature a = CallSignature { _callSignatureTypeParameters :: !a, _callSignatureParameters :: ![a], _callSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Ord1 CallSignature where liftCompare = genericLiftCompare
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { _constructSignatureTypeParameters :: !a, _constructSignatureParameters :: ![a], _constructSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Ord1 ConstructSignature where liftCompare = genericLiftCompare
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec

data IndexSignature a = IndexSignature { _indexSignatureSubject :: a, _indexSignatureType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Ord1 IndexSignature where liftCompare = genericLiftCompare
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec

data MethodSignature a = MethodSignature { _methodSignatureContext :: ![a], _methodSignatureName :: !a, _methodSignatureParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 MethodSignature where liftEq = genericLiftEq
instance Ord1 MethodSignature where liftCompare = genericLiftCompare
instance Show1 MethodSignature where liftShowsPrec = genericLiftShowsPrec

data Debugger a = Debugger
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Ord1 Debugger where liftCompare = genericLiftCompare
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec

data ForOf a = ForOf { _forOfBinding :: !a, _forOfSubject :: !a, _forOfBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Ord1 ForOf where liftCompare = genericLiftCompare
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec

data This a = This
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec

data LabeledStatement a = LabeledStatement { _labeledStatementIdentifier :: !a, _labeledStatementSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec

newtype Update a = Update { _updateSubject :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec

data ImportAlias a = ImportAlias { _importAliasSubject :: !a, _importAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec

data InternalModule a = InternalModule { _internalModuleIdentifier :: !a, _internalModuleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Ord1 InternalModule where liftCompare = genericLiftCompare
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec

data Undefined a = Undefined
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Ord1 Undefined where liftCompare = genericLiftCompare
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec

data ClassHeritage a = ClassHeritage { _classHeritageExtendsClause :: !a, _implementsClause :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Ord1 ClassHeritage where liftCompare = genericLiftCompare
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec

data AbstractClass a = AbstractClass { _abstractClassIdentifier :: !a,  _abstractClassTypeParameters :: !a, _classHeritage :: ![a], _classBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Ord1 AbstractClass where liftCompare = genericLiftCompare
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec

data JsxElement a = JsxElement { _jsxOpeningElement :: !a,  _jsxElements :: ![a], _jsxClosingElement :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxElement where liftEq = genericLiftEq
instance Ord1 JsxElement where liftCompare = genericLiftCompare
instance Show1 JsxElement where liftShowsPrec = genericLiftShowsPrec

newtype JsxText a = JsxText ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxText where liftEq = genericLiftEq
instance Ord1 JsxText where liftCompare = genericLiftCompare
instance Show1 JsxText where liftShowsPrec = genericLiftShowsPrec

newtype JsxExpression a = JsxExpression { _jsxExpression :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxExpression where liftEq = genericLiftEq
instance Ord1 JsxExpression where liftCompare = genericLiftCompare
instance Show1 JsxExpression where liftShowsPrec = genericLiftShowsPrec

data JsxOpeningElement a = JsxOpeningElement { _jsxOpeningElementIdentifier :: !a,  _jsxAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxOpeningElement where liftEq = genericLiftEq
instance Ord1 JsxOpeningElement where liftCompare = genericLiftCompare
instance Show1 JsxOpeningElement where liftShowsPrec = genericLiftShowsPrec

newtype JsxClosingElement a = JsxClosingElement { _jsxClosingElementIdentifier :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxClosingElement where liftEq = genericLiftEq
instance Ord1 JsxClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxClosingElement where liftShowsPrec = genericLiftShowsPrec

data JsxSelfClosingElement a = JsxSelfClosingElement { _jsxSelfClosingElementIdentifier :: !a, _jsxSelfClosingElementAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxSelfClosingElement where liftEq = genericLiftEq
instance Ord1 JsxSelfClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxSelfClosingElement where liftShowsPrec = genericLiftShowsPrec

data JsxAttribute a = JsxAttribute { _jsxAttributeTarget :: !a, _jsxAttributeValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 JsxAttribute where liftEq = genericLiftEq
instance Ord1 JsxAttribute where liftCompare = genericLiftCompare
instance Show1 JsxAttribute where liftShowsPrec = genericLiftShowsPrec

newtype ImplementsClause a = ImplementsClause { _implementsClauseTypes :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Ord1 ImplementsClause where liftCompare = genericLiftCompare
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec

data OptionalParameter a = OptionalParameter { _optionalParameterContext :: ![a], _optionalParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

data RequiredParameter a = RequiredParameter { _requiredParameterContext :: ![a], _requiredParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

data RestParameter a = RestParameter { _restParameterContext :: ![a], _restParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 RestParameter where liftEq = genericLiftEq
instance Ord1 RestParameter where liftCompare = genericLiftCompare
instance Show1 RestParameter where liftShowsPrec = genericLiftShowsPrec
