{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}

module Language.TypeScript.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Algorithm
import GHC.Generics
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.ByteString (ByteString)
import Data.Align.Generic
import Data.Maybe (fromMaybe)
import Data.Record
import Data.Maybe (catMaybes)
import Data.Syntax (emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm1, contextualize)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Stack
import Language.TypeScript.Grammar as Grammar
import qualified Term
import Data.List.NonEmpty (some1)
import Data.Function (on)
import Data.Foldable (toList)

-- | The type of TypeScript syntax.
type Syntax = '[
    Comment.Comment
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.Module
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Expression.Call
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.MemberAccess
  , Expression.ScopeResolution
  , Expression.Subscript
  , Expression.Delete
  , Expression.Void
  , Expression.Typeof
  , Expression.InstanceOf
  , Literal.Array
  , Literal.Boolean
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.String
  , Literal.TextElement
  , Literal.Regex
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.Else
  , Statement.Finally
  , Statement.For
  , Statement.ForEach
  , Statement.If
  , Statement.Match
  , Statement.Pattern
  , Statement.Retry
  , Statement.Return
  , Statement.ScopeEntry
  , Statement.ScopeExit
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Statement.Throw
  , Statement.DoWhile
  , Syntax.AccessibilityModifier
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Program
  , Syntax.Context
  , Type.Annotation
  , Type.Readonly
  , Type.TypeParameters
  , Language.TypeScript.Syntax.TypeParameter
  , Language.TypeScript.Syntax.Constraint
  , Language.TypeScript.Syntax.ParenthesizedType
  , Language.TypeScript.Syntax.PredefinedType
  , Language.TypeScript.Syntax.TypeIdentifier
  , Language.TypeScript.Syntax.NestedIdentifier
  , Language.TypeScript.Syntax.NestedTypeIdentifier
  , Language.TypeScript.Syntax.GenericType
  , Language.TypeScript.Syntax.TypeArguments
  , Language.TypeScript.Syntax.TypePredicate
  , Language.TypeScript.Syntax.Annotation
  , Language.TypeScript.Syntax.CallSignature
  , Language.TypeScript.Syntax.ConstructSignature
  , Language.TypeScript.Syntax.ArrayType
  , Language.TypeScript.Syntax.FlowMaybeType
  , Language.TypeScript.Syntax.TypeQuery
  , Language.TypeScript.Syntax.IndexTypeQuery
  , Language.TypeScript.Syntax.ThisType
  , Language.TypeScript.Syntax.ExistentialType
  , Language.TypeScript.Syntax.MethodSignature
  , Language.TypeScript.Syntax.IndexSignature
  , Language.TypeScript.Syntax.ObjectType
  , Language.TypeScript.Syntax.LiteralType
  , Language.TypeScript.Syntax.Union
  , Language.TypeScript.Syntax.Intersection
  , Language.TypeScript.Syntax.Function
  , Language.TypeScript.Syntax.Tuple
  , Language.TypeScript.Syntax.Constructor
  , Language.TypeScript.Syntax.TypeAssertion
  , Language.TypeScript.Syntax.Cast
  , Language.TypeScript.Syntax.ImportAlias
  , Language.TypeScript.Syntax.NonNullExpression
  , Language.TypeScript.Syntax.Debugger
  , Language.TypeScript.Syntax.ShorthandPropertyIdentifier
  , Language.TypeScript.Syntax.InternalModule
  , Language.TypeScript.Syntax.Super
  , Language.TypeScript.Syntax.Undefined
  , Language.TypeScript.Syntax.ClassHeritage
  , Language.TypeScript.Syntax.AbstractClass
  , Language.TypeScript.Syntax.ExtendsClause
  , Language.TypeScript.Syntax.ImplementsClause
  , Language.TypeScript.Syntax.JsxElement
  , Language.TypeScript.Syntax.JsxSelfClosingElement
  , Language.TypeScript.Syntax.JsxOpeningElement
  , Language.TypeScript.Syntax.JsxText
  , Language.TypeScript.Syntax.JsxClosingElement
  , Language.TypeScript.Syntax.JsxExpression
  , Language.TypeScript.Syntax.JsxAttribute
  , Language.TypeScript.Syntax.SequenceExpression
  , Language.TypeScript.Syntax.OptionalParameter
  , Language.TypeScript.Syntax.RequiredParameter
  , Language.TypeScript.Syntax.RestParameter
  , Language.TypeScript.Syntax.PropertySignature
  , Language.TypeScript.Syntax.ExpressionStatement
  , Language.TypeScript.Syntax.ImportExportSpecifier
  , Language.TypeScript.Syntax.ExportClause
  , Language.TypeScript.Syntax.Export
  , Language.TypeScript.Syntax.AmbientDeclaration
  , Language.TypeScript.Syntax.InterfaceDeclaration
  , Language.TypeScript.Syntax.EnumDeclaration
  , Language.TypeScript.Syntax.TypeAliasDeclaration
  , Language.TypeScript.Syntax.ExtendsClause
  , Language.TypeScript.Syntax.AmbientFunction
  , Language.TypeScript.Syntax.ImportRequireClause
  , Language.TypeScript.Syntax.ImportClause
  , Language.TypeScript.Syntax.LabeledStatement
  , Language.TypeScript.Syntax.NamedImports
  , Language.TypeScript.Syntax.NamespaceImport
  , Language.TypeScript.Syntax.Import
  , Language.TypeScript.Syntax.With
  , Language.TypeScript.Syntax.ForOf
  , Language.TypeScript.Syntax.This
  , Language.TypeScript.Syntax.New
  , Language.TypeScript.Syntax.Update
  , Language.TypeScript.Syntax.Await
  , Language.TypeScript.Syntax.PublicFieldDefinition
  , Language.TypeScript.Syntax.VariableDeclaration
  , Type.Visibility
  , []
  ]

type Term = Term.Term (Data.Union.Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
data ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec

data Union a = Union { unionLeft :: !a, unionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Language.TypeScript.Syntax.Union where liftEq = genericLiftEq
instance Show1 Language.TypeScript.Syntax.Union where liftShowsPrec = genericLiftShowsPrec

data Intersection a = Intersection { intersectionLeft :: !a, intersectionRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec

data PublicFieldDefinition a = PublicFieldDefinition { publicFieldContext :: ![a], publicFieldPropertyName :: !a, publicFieldValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 PublicFieldDefinition where liftEq = genericLiftEq
instance Show1 PublicFieldDefinition where liftShowsPrec = genericLiftShowsPrec

data Function a = Function { functionTypeParameters :: !a, functionFormalParameters :: ![a], functionType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

data AmbientFunction a = AmbientFunction { ambientFunctionContext :: ![a], ambientFunctionIdentifier :: !a, ambientFunctionParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec

data ImportRequireClause a = ImportRequireClause { importRequireIdentifier :: !a, importRequireSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImportRequireClause where liftEq = genericLiftEq
instance Show1 ImportRequireClause where liftShowsPrec = genericLiftShowsPrec

data ImportClause a = ImportClause { importClauseElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImportClause where liftEq = genericLiftEq
instance Show1 ImportClause where liftShowsPrec = genericLiftShowsPrec

data NamespaceImport a = NamespaceImport { namespaceImportSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NamespaceImport where liftEq = genericLiftEq
instance Show1 NamespaceImport where liftShowsPrec = genericLiftShowsPrec

data Tuple a = Tuple { tupleElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

data Constructor a = Constructor { constructorTypeParameters :: !a, constructorFormalParameters :: ![a], constructorType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Language.TypeScript.Syntax.Constructor where liftEq = genericLiftEq
instance Show1 Language.TypeScript.Syntax.Constructor where liftShowsPrec = genericLiftShowsPrec

data TypeParameter a = TypeParameter { typeParameter :: !a, typeParameterConstraint :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec

data TypeAssertion a = TypeAssertion { typeAssertionParameters :: !a, typeAssertionExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeAssertion where liftEq = genericLiftEq
instance Show1 TypeAssertion where liftShowsPrec = genericLiftShowsPrec

data Cast a =  Cast { castSubject :: !a, castType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Cast where liftEq = genericLiftEq
instance Show1 Cast where liftShowsPrec = genericLiftShowsPrec

data Annotation a = Annotation { typeAnnotation :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

data Constraint a = Constraint { constraintType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec

data ParenthesizedType a = ParenthesizedType { parenthesizedType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec

data PredefinedType a = PredefinedType { predefinedType :: !ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec

data TypeIdentifier a = TypeIdentifier ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec

data NestedIdentifier a = NestedIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec

data NestedTypeIdentifier a = NestedTypeIdentifier !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NestedTypeIdentifier where liftEq = genericLiftEq
instance Show1 NestedTypeIdentifier where liftShowsPrec = genericLiftShowsPrec

data GenericType a = GenericType { genericTypeIdentifier :: !a, genericTypeArguments :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec

data TypePredicate a = TypePredicate { typePredicateIdentifier :: !a, typePredicateType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypePredicate where liftEq = genericLiftEq
instance Show1 TypePredicate where liftShowsPrec = genericLiftShowsPrec

data ObjectType a = ObjectType { objectTypeElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ObjectType where liftEq = genericLiftEq
instance Show1 ObjectType where liftShowsPrec = genericLiftShowsPrec

data Export a = Export { exportElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Export where liftEq = genericLiftEq
instance Show1 Export where liftShowsPrec = genericLiftShowsPrec

data ExportClause a = ExportClause { exportClauseElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ExportClause where liftEq = genericLiftEq
instance Show1 ExportClause where liftShowsPrec = genericLiftShowsPrec

data ImportExportSpecifier a = ImportExportSpecifier { specifierSubject :: !a, specifierAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImportExportSpecifier where liftEq = genericLiftEq
instance Show1 ImportExportSpecifier where liftShowsPrec = genericLiftShowsPrec

data NamedImports a = NamedImports { namedImportElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NamedImports where liftEq = genericLiftEq
instance Show1 NamedImports where liftShowsPrec = genericLiftShowsPrec

data With a = With { withExpression :: !a, withBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 With where liftEq = genericLiftEq
instance Show1 With where liftShowsPrec = genericLiftShowsPrec

data AmbientDeclaration a = AmbientDeclaration { ambientDeclarationBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec

data InterfaceDeclaration a = InterfaceDeclaration { interfaceDeclarationContext :: ![a], interfaceDeclarationIdentifier :: !a, interfaceDeclarationBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 InterfaceDeclaration where liftEq = genericLiftEq
instance Show1 InterfaceDeclaration where liftShowsPrec = genericLiftShowsPrec

data EnumDeclaration a = EnumDeclaration { enumDeclarationIdentifier :: !a, enumDeclarationBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec

data TypeAliasDeclaration a = TypeAliasDeclaration { typeAliasDeclarationContext :: ![a], typeAliasDeclarationIdentifier :: !a, typeAliasDeclarationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeAliasDeclaration where liftEq = genericLiftEq
instance Show1 TypeAliasDeclaration where liftShowsPrec = genericLiftShowsPrec

data ExtendsClause a = ExtendsClause { extendsClauseTypes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec

data Import a = Import { importElements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

data ArrayType a = ArrayType { arrayType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec

data FlowMaybeType a = FlowMaybeType { flowMaybeType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 FlowMaybeType where liftEq = genericLiftEq
instance Show1 FlowMaybeType where liftShowsPrec = genericLiftShowsPrec

data TypeQuery a = TypeQuery { typeQuerySubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeQuery where liftEq = genericLiftEq
instance Show1 TypeQuery where liftShowsPrec = genericLiftShowsPrec

data IndexTypeQuery a = IndexTypeQuery { indexTypeQuerySubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 IndexTypeQuery where liftEq = genericLiftEq
instance Show1 IndexTypeQuery where liftShowsPrec = genericLiftShowsPrec

data TypeArguments a = TypeArguments { typeArguments :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeArguments where liftEq = genericLiftEq
instance Show1 TypeArguments where liftShowsPrec = genericLiftShowsPrec

data ThisType a = ThisType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ThisType where liftEq = genericLiftEq
instance Show1 ThisType where liftShowsPrec = genericLiftShowsPrec

data ExistentialType a = ExistentialType ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ExistentialType where liftEq = genericLiftEq
instance Show1 ExistentialType where liftShowsPrec = genericLiftShowsPrec

data LiteralType a = LiteralType { literalTypeSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 LiteralType where liftEq = genericLiftEq
instance Show1 LiteralType where liftShowsPrec = genericLiftShowsPrec

data PropertySignature a = PropertySignature { modifiers :: ![a], propertySignaturePropertyName :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec

data CallSignature a = CallSignature { callSignatureTypeParameters :: !a, callSignatureParameters :: ![a], callSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { constructSignatureTypeParameters :: !a, constructSignatureParameters :: ![a], constructSignatureType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec

data IndexSignature a = IndexSignature { indexSignatureSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec

data MethodSignature a = MethodSignature { methodSignatureContext :: ![a], methodSignatureName :: !a, methodSignatureParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 MethodSignature where liftEq = genericLiftEq
instance Show1 MethodSignature where liftShowsPrec = genericLiftShowsPrec

data Debugger a = Debugger
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec

data ExpressionStatement a = ExpressionStatement { expressionStatement :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ExpressionStatement where liftEq = genericLiftEq
instance Show1 ExpressionStatement where liftShowsPrec = genericLiftShowsPrec

data ForOf a = ForOf { forOfBinding :: !a, forOfSubject :: !a, forOfBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec

data This a = This
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 This where liftEq = genericLiftEq
instance Show1 This where liftShowsPrec = genericLiftShowsPrec

data LabeledStatement a = LabeledStatement { labeledStatementIdentifier :: !a, labeledStatementSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec

data NonNullExpression a = NonNullExpression { nonNullExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

data Update a = Update { updateSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec

data Await a = Await { awaitSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Await where liftEq = genericLiftEq
instance Show1 Await where liftShowsPrec = genericLiftShowsPrec

data New a = New { newSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 New where liftEq = genericLiftEq
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

data ImportAlias a = ImportAlias { importAliasSubject :: !a, importAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec

data InternalModule a = InternalModule { internalModuleIdentifier :: !a, internalModuleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec

data Super a = Super
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Super where liftEq = genericLiftEq
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec

data Undefined a = Undefined
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec

data ClassHeritage a = ClassHeritage { classHeritageExtendsClause :: !a, implementsClause :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec

data AbstractClass a = AbstractClass { abstractClassIdentifier :: !a,  abstractClassTypeParameters :: !a, classHeritage :: ![a], classBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec

data JsxElement a = JsxElement { jsxOpeningElement :: !a,  jsxElements :: ![a], jsxClosingElement :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxElement where liftEq = genericLiftEq
instance Show1 JsxElement where liftShowsPrec = genericLiftShowsPrec

data JsxText a = JsxText ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxText where liftEq = genericLiftEq
instance Show1 JsxText where liftShowsPrec = genericLiftShowsPrec

data JsxExpression a = JsxExpression { jsxExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxExpression where liftEq = genericLiftEq
instance Show1 JsxExpression where liftShowsPrec = genericLiftShowsPrec

data JsxOpeningElement a = JsxOpeningElement { jsxOpeningElementIdentifier :: !a,  jsxAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxOpeningElement where liftEq = genericLiftEq
instance Show1 JsxOpeningElement where liftShowsPrec = genericLiftShowsPrec

data JsxClosingElement a = JsxClosingElement { jsxClosingElementIdentifier :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxClosingElement where liftEq = genericLiftEq
instance Show1 JsxClosingElement where liftShowsPrec = genericLiftShowsPrec

data JsxSelfClosingElement a = JsxSelfClosingElement { jsxSelfClosingElementIdentifier :: !a, jsxSelfClosingElementAttributes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxSelfClosingElement where liftEq = genericLiftEq
instance Show1 JsxSelfClosingElement where liftShowsPrec = genericLiftShowsPrec

data JsxAttribute a = JsxAttribute { jsxAttributeTarget :: !a, jsxAttributeValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 JsxAttribute where liftEq = genericLiftEq
instance Show1 JsxAttribute where liftShowsPrec = genericLiftShowsPrec

data ImplementsClause a = ImplementsClause { implementsClauseTypes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec

data SequenceExpression a = SequenceExpression { firstExpression :: !a, secondExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 SequenceExpression where liftEq = genericLiftEq
instance Show1 SequenceExpression where liftShowsPrec = genericLiftShowsPrec

data OptionalParameter a = OptionalParameter { optionalParameterContext :: ![a], optionalParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

data RequiredParameter a = RequiredParameter { requiredParameterContext :: ![a], requiredParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

data RestParameter a = RestParameter { restParameterContext :: ![a], restParameterSubject :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 RestParameter where liftEq = genericLiftEq
instance Show1 RestParameter where liftShowsPrec = genericLiftShowsPrec

data VariableDeclaration a = VariableDeclaration { variableDeclarations :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 VariableDeclaration where liftEq = genericLiftEq
instance Show1 VariableDeclaration where liftShowsPrec = genericLiftShowsPrec

-- | Assignment from AST in Ruby’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = makeTerm <$> symbol Program <*> children (Syntax.Program <$> many statement)

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
term :: Assignment -> Assignment
term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)

expression :: Assignment
expression = everything
  where
    everything = choice [
      typeAssertion,
      asExpression,
      nonNullExpression',
      importAlias',
      internalModule,
      super,
      abstractClass,
      object,
      array,
      jsxElement,
      jsxSelfClosingElement,
      class',
      anonymousClass,
      function,
      arrowFunction,
      assignmentExpression,
      augmentedAssignmentExpression,
      awaitExpression,
      unaryExpression,
      binaryExpression,
      ternaryExpression,
      updateExpression,
      callExpression,
      memberExpression,
      newExpression,
      parenthesizedExpression,
      subscriptExpression,
      yieldExpression,
      thisExpression,
      number,
      string,
      templateString,
      regex,
      true,
      false,
      null',
      undefined',
      identifier
      ]

undefined' :: Assignment
undefined' = makeTerm <$> symbol Grammar.Undefined <*> (Language.TypeScript.Syntax.Undefined <$ source)

assignmentExpression :: Assignment
assignmentExpression = makeTerm <$> symbol AssignmentExpression <*> children (Statement.Assignment [] <$> (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) <*> expression)

augmentedAssignmentExpression :: Assignment
augmentedAssignmentExpression = makeTerm' <$> symbol AugmentedAssignmentExpression <*> children (infixTerm (memberExpression <|> subscriptExpression <|> identifier <|> destructuringPattern) expression [
  assign Expression.Plus <$ symbol AnonPlusEqual
  , assign Expression.Minus <$ symbol AnonMinusEqual
  , assign Expression.Times <$ symbol AnonStarEqual
  , assign Expression.DividedBy <$ symbol AnonSlashEqual
  , assign Expression.Modulo <$ symbol AnonPercentEqual
  , assign Expression.BXOr <$ symbol AnonCaretEqual
  , assign Expression.BAnd <$ symbol AnonAmpersandEqual
  , assign Expression.BOr <$ symbol AnonPipeEqual ])
  where assign :: f :< Syntax => (Term -> Term -> f Term) -> Term -> Term -> Data.Union.Union Syntax Term
        assign c l r = inj (Statement.Assignment [] l (makeTerm1 (c l r)))


awaitExpression :: Assignment
awaitExpression = makeTerm <$> symbol Grammar.AwaitExpression <*> children (Language.TypeScript.Syntax.Await <$> expression)

unaryExpression :: Assignment
unaryExpression = symbol Grammar.UnaryExpression >>= \ loc ->
  makeTerm loc . Expression.Not <$> children ((symbol AnonTilde <|> symbol AnonBang) *> expression)
  <|> makeTerm loc . Expression.Negate <$> children ((symbol AnonMinus <|> symbol AnonPlus) *> expression)
  <|> makeTerm loc . Expression.Typeof <$> children (symbol AnonTypeof *> expression)
  <|> makeTerm loc . Expression.Void <$> children (symbol AnonVoid *> expression)
  <|> makeTerm loc . Expression.Delete <$> children (symbol AnonDelete *> expression)

ternaryExpression :: Assignment
ternaryExpression = makeTerm <$> symbol Grammar.TernaryExpression <*> children (Statement.If <$> expression <*> expression <*> expression)

memberExpression :: Assignment
memberExpression = makeTerm <$> symbol Grammar.MemberExpression <*> children (Expression.MemberAccess <$> expression <*> propertyIdentifier)

newExpression :: Assignment
newExpression = makeTerm <$> symbol Grammar.NewExpression <*> children (Language.TypeScript.Syntax.New <$> expression)

updateExpression :: Assignment
updateExpression = makeTerm <$> symbol Grammar.UpdateExpression <*> children (Language.TypeScript.Syntax.Update <$> expression)

yieldExpression :: Assignment
yieldExpression = makeTerm <$> symbol Grammar.YieldExpression <*> children (Statement.Yield <$> (expression <|> emptyTerm))

thisExpression :: Assignment
thisExpression = makeTerm <$> symbol Grammar.ThisExpression <*> (Language.TypeScript.Syntax.This <$ source)

regex :: Assignment
regex = makeTerm <$> symbol Grammar.Regex <*> (Literal.Regex <$> source)

null' :: Assignment
null' = makeTerm <$> symbol Null <*> (Literal.Null <$ source)

anonymousClass :: Assignment
anonymousClass = makeTerm <$> symbol Grammar.AnonymousClass <*> children (Declaration.Class <$> pure [] <*> emptyTerm <*> (classHeritage' <|> pure []) <*> classBodyStatements)

abstractClass :: Assignment
abstractClass = makeTerm <$> symbol Grammar.AbstractClass <*> (Language.TypeScript.Syntax.AbstractClass <$> identifier <*> (typeParameters <|> emptyTerm) <*> (classHeritage' <|> pure []) <*> classBodyStatements)

classHeritage' :: HasCallStack => Assignment.Assignment [] Grammar [Term]
classHeritage' = symbol Grammar.ClassHeritage *> children (((++) `on` toList) <$> optional extendsClause' <*> optional implementsClause')

extendsClause' :: Assignment
extendsClause' = makeTerm <$> symbol Grammar.ExtendsClause <*> children (Language.TypeScript.Syntax.ExtendsClause <$> many ty)

implementsClause' :: Assignment
implementsClause' = makeTerm <$> symbol Grammar.ImplementsClause <*> children (Language.TypeScript.Syntax.ImplementsClause <$> many ty)

super :: Assignment
super = makeTerm <$> symbol Grammar.Super <*> (Language.TypeScript.Syntax.Super <$ source)

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol Grammar.TypeAssertion <*> children (Language.TypeScript.Syntax.TypeAssertion <$> typeArguments' <*> expression)

asExpression :: Assignment
asExpression = makeTerm <$> symbol AsExpression <*> children (Language.TypeScript.Syntax.Cast <$> expression <*> (ty <|> templateString))

templateString :: Assignment
templateString = makeTerm <$> symbol TemplateString <*> children (Literal.String <$> many templateSubstitution)

templateSubstitution :: Assignment
templateSubstitution = symbol TemplateSubstitution *> children expression

nonNullExpression' :: Assignment
nonNullExpression' = makeTerm <$> symbol Grammar.NonNullExpression <*> children (Language.TypeScript.Syntax.NonNullExpression <$> expression)

importAlias' :: Assignment
importAlias' = makeTerm <$> symbol Grammar.ImportAlias <*> children (Language.TypeScript.Syntax.ImportAlias <$> identifier <*> (identifier <|> nestedIdentifier))

number :: Assignment
number = makeTerm <$> symbol Grammar.Number <*> (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol Grammar.String <*> (Literal.TextElement <$> source)

true :: Assignment
true = makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)

false :: Assignment
false = makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

identifier :: Assignment
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

literal :: Assignment
literal =
      makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)
  <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)
  <|> makeTerm <$> symbol Grammar.Number <*> (Literal.Float <$> source)
  <|> makeTerm <$> symbol Grammar.Null <*> (Literal.Null <$ source)
   -- TODO: Do we want to represent the difference between .. and ...
  <|> makeTerm <$> symbol Array <*> children (Literal.Array <$> many expression)
  <|> makeTerm <$> symbol Object <*> children (Literal.Hash <$> many pair)
  -- TODO: Handle interpolation
  <|> makeTerm <$> symbol String <*> (Literal.TextElement <$> source)
  -- TODO: Handle interpolation, dedicated literal?
  <|> makeTerm <$> symbol Regex <*> (Literal.TextElement <$> source)

class' :: Assignment
class' = makeClass <$> symbol Class <*> children ((,,,) <$> identifier <*> ((symbol TypeParameters *> children (many typeParameter')) <|> pure []) <*> (classHeritage' <|> pure []) <*> classBodyStatements)
  where makeClass loc (expression, typeParams, classHeritage, statements) = makeTerm loc (Declaration.Class typeParams expression classHeritage statements)

object :: Assignment
object = makeTerm <$> symbol Object <*> children (Literal.Hash <$> many (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))

array :: Assignment
array = makeTerm <$> symbol Array <*> children (Literal.Array <$> many (expression <|> spreadElement))

jsxElement :: Assignment
jsxElement = makeTerm <$> symbol Grammar.JsxElement <*> children (Language.TypeScript.Syntax.JsxElement <$> jsxOpeningElement' <*> many (jsxElement <|> jsxSelfClosingElement <|> jsxExpression' <|> jsxText) <*> jsxClosingElement')

jsxSelfClosingElement :: Assignment
jsxSelfClosingElement = makeTerm <$> symbol Grammar.JsxSelfClosingElement <*> children (Language.TypeScript.Syntax.JsxSelfClosingElement <$> identifier <*> many jsxAttribute)

jsxOpeningElement' :: Assignment
jsxOpeningElement' = makeTerm <$> symbol Grammar.JsxOpeningElement <*> children (Language.TypeScript.Syntax.JsxOpeningElement <$> identifier <*> many jsxAttribute)

jsxExpression' :: Assignment
jsxExpression' = makeTerm <$> symbol Grammar.JsxExpression <*> children (Language.TypeScript.Syntax.JsxExpression <$> (expression <|> sequenceExpression <|> spreadElement))

jsxText :: Assignment
jsxText = makeTerm <$> symbol Grammar.JsxText <*> (Language.TypeScript.Syntax.JsxText <$> source)

jsxClosingElement' :: Assignment
jsxClosingElement' = makeTerm <$> symbol Grammar.JsxClosingElement <*> children (Language.TypeScript.Syntax.JsxClosingElement <$> identifier)

jsxAttribute :: Assignment
jsxAttribute = makeTerm <$> symbol Grammar.JsxAttribute <*> children (Language.TypeScript.Syntax.JsxAttribute <$> propertyIdentifier <*> (number <|> string <|> jsxExpression'))

propertyIdentifier :: Assignment
propertyIdentifier = makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier <$> source)

sequenceExpression :: Assignment
sequenceExpression = makeTerm <$> symbol Grammar.SequenceExpression <*> children (Language.TypeScript.Syntax.SequenceExpression <$> expression <*> (sequenceExpression <|> expression))

parameter :: Assignment
parameter =
      requiredParameter
  <|> restParameter
  <|> optionalParameter

accessibilityModifier' :: Assignment
accessibilityModifier' = makeTerm <$> symbol AccessibilityModifier <*> children (Syntax.Identifier <$> source)

destructuringPattern :: Assignment
destructuringPattern = makeTerm <$> symbol ObjectPattern <*> children (Literal.Hash <$> many (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))

spreadElement :: Assignment
spreadElement = symbol SpreadElement *> children expression

readonly' :: Assignment
readonly' = makeTerm <$> symbol Readonly <*> (Type.Readonly <$ source)

methodDefinition :: Assignment
methodDefinition = makeMethod <$>
  symbol MethodDefinition
  <*> children ((,,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> emptyTerm <*> propertyName <*> callSignatureParts <*> statementBlock)
  where
    makeMethod loc (modifier, readonly, receiver, propertyName', (typeParameters', params, ty'), statements) = makeTerm loc (Declaration.Method [modifier, readonly, typeParameters', ty'] receiver propertyName' params statements)

callSignatureParts :: HasCallStack => Assignment.Assignment [] Grammar (Term, [Term], Term)
callSignatureParts =  symbol Grammar.CallSignature *> children ((,,) <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

callSignature :: Assignment
callSignature =  makeTerm <$> symbol Grammar.CallSignature <*> children (Language.TypeScript.Syntax.CallSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

constructSignature :: Assignment
constructSignature = makeTerm <$> symbol Grammar.ConstructSignature <*> children (Language.TypeScript.Syntax.ConstructSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

indexSignature :: Assignment
indexSignature = makeTerm <$> symbol Grammar.IndexSignature <*> children (Language.TypeScript.Syntax.IndexSignature <$> (identifier <|> typeAnnotation'))

methodSignature :: Assignment
methodSignature = makeMethodSignature <$> symbol Grammar.MethodSignature <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> callSignatureParts)
  where makeMethodSignature loc (modifier, readonly, propertyName, (typeParams, params, annotation)) = makeTerm loc (Language.TypeScript.Syntax.MethodSignature [modifier, readonly, typeParams, annotation] propertyName params)

formalParameters :: HasCallStack => Assignment.Assignment [] Grammar [Term]
formalParameters = symbol FormalParameters *> children (many parameter)

typeParameters :: Assignment
typeParameters = makeTerm <$> symbol TypeParameters <*> children (Type.TypeParameters <$> many typeParameter')

typeAnnotation' :: Assignment
typeAnnotation' = makeTerm <$> symbol TypeAnnotation <*> children (Language.TypeScript.Syntax.Annotation <$> ty)

typeParameter' :: Assignment
typeParameter' = makeTerm <$> symbol Grammar.TypeParameter <*> children (Language.TypeScript.Syntax.TypeParameter <$> identifier <*> (constraint <|> emptyTerm))

constraint :: Assignment
constraint = makeTerm <$> symbol Grammar.Constraint <*> children (Language.TypeScript.Syntax.Constraint <$> ty)

function :: Assignment
function = makeFunction <$> (symbol Grammar.Function <|> symbol Grammar.GeneratorFunction) <*> children ((,,) <$> (identifier <|> emptyTerm) <*> callSignatureParts <*> statementBlock)
  where makeFunction loc (id, (typeParams, params, annotation), statements) = makeTerm loc (Declaration.Function [typeParams, annotation] id params statements)

ambientFunction :: Assignment
ambientFunction = makeAmbientFunction <$> symbol Grammar.AmbientFunction <*> children ((,) <$> identifier <*> callSignatureParts)
  where makeAmbientFunction loc (id, (typeParams, params, annotation)) = makeTerm loc (Language.TypeScript.Syntax.AmbientFunction [typeParams, annotation] id params)

ty :: Assignment
ty = primaryType <|> unionType <|> intersectionType <|> functionTy <|> constructorTy

primaryType :: Assignment
primaryType = parenthesizedTy <|> predefinedTy <|> typeIdentifier <|> nestedTypeIdentifier <|> genericType <|> typePredicate <|> objectType <|> arrayTy <|> tupleType <|> flowMaybeTy <|> typeQuery <|> indexTypeQuery <|> thisType <|> existentialType <|> literalType

parenthesizedTy :: Assignment
parenthesizedTy = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Language.TypeScript.Syntax.ParenthesizedType <$> ty)

predefinedTy :: Assignment
predefinedTy = makeTerm <$> symbol Grammar.PredefinedType <*> (Language.TypeScript.Syntax.PredefinedType <$> source)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol Grammar.TypeIdentifier <*> (Language.TypeScript.Syntax.TypeIdentifier <$> source)

nestedIdentifier :: Assignment
nestedIdentifier = makeTerm <$> symbol Grammar.NestedIdentifier <*> children (Language.TypeScript.Syntax.NestedIdentifier <$> (identifier <|> nestedIdentifier) <*> identifier)

nestedTypeIdentifier :: Assignment
nestedTypeIdentifier = makeTerm <$> symbol Grammar.NestedTypeIdentifier <*> children (Language.TypeScript.Syntax.NestedTypeIdentifier <$> (identifier <|> nestedIdentifier) <*> typeIdentifier)

genericType :: Assignment
genericType = makeTerm <$> symbol Grammar.GenericType <*> children (Language.TypeScript.Syntax.GenericType <$> (typeIdentifier <|> nestedTypeIdentifier) <*> typeArguments')

typeArguments' :: Assignment
typeArguments' = makeTerm <$> symbol Grammar.TypeArguments <*> children (Language.TypeScript.Syntax.TypeArguments <$> some ty)

typePredicate :: Assignment
typePredicate = makeTerm <$> symbol Grammar.TypePredicate <*> children (Language.TypeScript.Syntax.TypePredicate <$> identifier <*> ty)

objectType :: Assignment
objectType = makeTerm <$> symbol Grammar.ObjectType <*> children (Language.TypeScript.Syntax.ObjectType <$> many (exportStatement <|> propertySignature <|> callSignature <|> constructSignature <|> indexSignature <|> methodSignature))

arrayTy :: Assignment
arrayTy = makeTerm <$> symbol Grammar.ArrayType <*> children (Language.TypeScript.Syntax.ArrayType <$> ty)

flowMaybeTy :: Assignment
flowMaybeTy = makeTerm <$> symbol Grammar.FlowMaybeType <*> children (Language.TypeScript.Syntax.FlowMaybeType <$> primaryType)

typeQuery :: Assignment
typeQuery = makeTerm <$> symbol Grammar.TypeQuery <*> children (Language.TypeScript.Syntax.TypeQuery <$> (identifier <|> nestedIdentifier))

indexTypeQuery :: Assignment
indexTypeQuery = makeTerm <$> symbol Grammar.IndexTypeQuery <*> children (Language.TypeScript.Syntax.IndexTypeQuery <$> (identifier <|> nestedIdentifier))

thisType :: Assignment
thisType = makeTerm <$> symbol Grammar.ThisType <*> (Language.TypeScript.Syntax.ThisType <$> source)

existentialType :: Assignment
existentialType = makeTerm <$> symbol Grammar.ExistentialType <*> (Language.TypeScript.Syntax.ExistentialType <$> source)

literalType :: Assignment
literalType = makeTerm <$> symbol Grammar.LiteralType <*> children (Language.TypeScript.Syntax.LiteralType <$> (number <|> string <|> true <|> false))

unionType :: Assignment
unionType = makeTerm <$> symbol UnionType <*> children (Language.TypeScript.Syntax.Union <$> ty <*> ty)

intersectionType :: Assignment
intersectionType = makeTerm <$> symbol IntersectionType <*> children (Language.TypeScript.Syntax.Intersection <$> ty <*> ty)

functionTy :: Assignment
functionTy = makeTerm <$> symbol FunctionType <*> children (Language.TypeScript.Syntax.Function <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> ty)

tupleType :: Assignment
tupleType = makeTerm <$> symbol TupleType <*> children (Language.TypeScript.Syntax.Tuple <$> many ty)

constructorTy :: Assignment
constructorTy = makeTerm <$> symbol ConstructorType <*> children (Language.TypeScript.Syntax.Constructor <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> ty)

statementBlock :: Assignment
statementBlock = makeTerm <$> symbol StatementBlock <*> children (many statement)

classBodyStatements :: HasCallStack => Assignment.Assignment [] Grammar [Term]
classBodyStatements = symbol ClassBody *> children (many (methodDefinition <|> publicFieldDefinition))

publicFieldDefinition :: Assignment
publicFieldDefinition = makeField <$> symbol Grammar.PublicFieldDefinition <*> children ((,,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeField loc (modifier, readonly, propertyName, annotation, expression) = makeTerm loc (Language.TypeScript.Syntax.PublicFieldDefinition [modifier, readonly, annotation] propertyName expression)


statement :: Assignment
statement = term (handleError everything)
  where
    everything = choice [
      exportStatement
      , importStatement
      , debuggerStatement
      , expressionStatement'
      , declaration
      , statementBlock
      , ifStatement
      , switchStatement
      , forStatement
      , forInStatement
      , forOfStatement
      , whileStatement
      , doStatement
      , tryStatement
      , withStatement
      , breakStatement
      , continueStatement
      , returnStatement
      , throwStatement
      , emptyStatement
      , labeledStatement ]

forOfStatement :: Assignment
forOfStatement = makeTerm <$> symbol ForOfStatement <*> children (Language.TypeScript.Syntax.ForOf <$> expression <*> expression <*> statement)

forInStatement :: Assignment
forInStatement = makeTerm <$> symbol ForInStatement <*> children (Statement.ForEach <$> expression <*> expression <*> statement)

doStatement :: Assignment
doStatement = makeTerm <$> symbol DoStatement <*> children (flip Statement.DoWhile <$> statementBlock <*> parenthesizedExpression)

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> ((symbol StatementIdentifier *> identifier) <|> emptyTerm))

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> ((symbol StatementIdentifier *> identifier) <|> emptyTerm))

withStatement :: Assignment
withStatement = makeTerm <$> symbol WithStatement <*> children (Language.TypeScript.Syntax.With <$> parenthesizedExpression <*> statement)

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expression <|> sequenceExpression <|> emptyTerm))

throwStatement :: Assignment
throwStatement = makeTerm <$> symbol Grammar.ThrowStatement <*> children (Statement.Throw <$> (expression <|> sequenceExpression))

labeledStatement :: Assignment
labeledStatement = makeTerm <$> symbol Grammar.LabeledStatement <*> children (Language.TypeScript.Syntax.LabeledStatement <$> (symbol StatementIdentifier *> children identifier) <*> statement)

importStatement :: Assignment
importStatement = makeTerm <$> symbol Grammar.ImportStatement <*> children (Language.TypeScript.Syntax.Import <$> (((\a b -> [a, b]) <$> importClause <*> fromClause) <|> (pure <$> (importRequireClause <|> string))))

importClause :: Assignment
importClause = makeTerm <$> symbol Grammar.ImportClause <*> children (Language.TypeScript.Syntax.ImportClause <$> (((\a b -> [a, b]) <$> identifier <*> (namespaceImport <|> namedImports)) <|> (pure <$> (namespaceImport <|> namedImports <|> identifier))))

namedImports :: Assignment
namedImports = makeTerm <$> symbol Grammar.NamedImports <*> children (Language.TypeScript.Syntax.NamedImports <$> many importExportSpecifier)

namespaceImport :: Assignment
namespaceImport = makeTerm <$> symbol Grammar.NamespaceImport <*> children (Language.TypeScript.Syntax.NamespaceImport <$> identifier)

importRequireClause :: Assignment
importRequireClause = makeTerm <$> symbol Grammar.ImportRequireClause <*> children (Language.TypeScript.Syntax.ImportRequireClause <$> identifier <*> string)

debuggerStatement :: Assignment
debuggerStatement = makeTerm <$> symbol Grammar.DebuggerStatement <*> (Language.TypeScript.Syntax.Debugger <$ source)

expressionStatement' :: Assignment
expressionStatement' = makeTerm <$> symbol Grammar.ExpressionStatement <*> children (Language.TypeScript.Syntax.ExpressionStatement <$> (expression <|> sequenceExpression))

declaration :: Assignment
declaration = everything
  where
    everything = choice [
      exportStatement,
      importAlias',
      function,
      internalModule,
      ambientFunction,
      class',
      module',
      variableDeclaration,
      typeAliasDeclaration,
      enumDeclaration,
      interfaceDeclaration,
      ambientDeclaration
      ]

typeAliasDeclaration :: Assignment
typeAliasDeclaration = makeTypeAliasDecl <$> symbol Grammar.TypeAliasDeclaration <*> children ((,,) <$> identifier <*> (typeParameters <|> emptyTerm) <*> ty)
  where makeTypeAliasDecl loc (identifier, typeParams, body) = makeTerm loc (Language.TypeScript.Syntax.TypeAliasDeclaration [typeParams] identifier body)

enumDeclaration :: Assignment
enumDeclaration = makeTerm <$> symbol Grammar.EnumDeclaration <*> children (Language.TypeScript.Syntax.EnumDeclaration <$> identifier <*> many (propertyName <|> enumAssignment))

enumAssignment :: Assignment
enumAssignment = makeTerm <$> symbol Grammar.EnumAssignment <*> children (Statement.Assignment [] <$> propertyName <*> expression)

interfaceDeclaration :: Assignment
interfaceDeclaration = makeInterfaceDecl <$> symbol Grammar.InterfaceDeclaration <*> children ((,,,) <$> identifier <*> (typeParameters <|> emptyTerm) <*> (extendsClause <|> emptyTerm) <*> objectType)
  where makeInterfaceDecl loc (identifier, typeParams, clause, objectType) = makeTerm loc (Language.TypeScript.Syntax.InterfaceDeclaration [typeParams, clause] identifier objectType)

extendsClause :: Assignment
extendsClause = makeTerm <$> symbol Grammar.ExtendsClause <*> children (Language.TypeScript.Syntax.ExtendsClause <$> many ty)

ambientDeclaration :: Assignment
ambientDeclaration = makeTerm <$> symbol Grammar.AmbientDeclaration <*> children (Language.TypeScript.Syntax.AmbientDeclaration <$> choice [declaration, statementBlock])

exportStatement :: Assignment
exportStatement = makeTerm <$> symbol Grammar.ExportStatement <*> children (Language.TypeScript.Syntax.Export <$> (((\a b -> [a, b]) <$> exportClause <*> fromClause) <|> (pure <$> (fromClause <|> exportClause <|> declaration <|> expression <|> identifier <|> importAlias'))))

fromClause :: Assignment
fromClause = string

exportClause :: Assignment
exportClause = makeTerm <$> symbol Grammar.ExportClause <*> children (Language.TypeScript.Syntax.ExportClause <$> many importExportSpecifier)

importExportSpecifier :: Assignment
importExportSpecifier = makeTerm <$> (symbol Grammar.ExportSpecifier <|> symbol Grammar.ImportSpecifier) <*> children (Language.TypeScript.Syntax.ImportExportSpecifier <$> identifier <*> (identifier <|> emptyTerm))

propertySignature :: Assignment
propertySignature = makePropertySignature <$> symbol Grammar.PropertySignature <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (readonly' <|> emptyTerm) <*> propertyName <*> (typeAnnotation' <|> emptyTerm))
  where makePropertySignature loc (modifier, readonly, propertyName, annotation) = makeTerm loc (Language.TypeScript.Syntax.PropertySignature [modifier, readonly, annotation] propertyName)

propertyName :: Assignment
propertyName = (makeTerm <$> symbol PropertyIdentifier <*> (Syntax.Identifier <$> source)) <|> string <|> number

assignmentPattern :: Assignment
assignmentPattern = makeTerm <$> symbol AssignmentPattern <*> children (Statement.Assignment [] <$> shorthandPropertyIdentifier <*> expression)

shorthandPropertyIdentifier :: Assignment
shorthandPropertyIdentifier = makeTerm <$> symbol Grammar.ShorthandPropertyIdentifier <*> (Language.TypeScript.Syntax.ShorthandPropertyIdentifier <$> source)

requiredParameter :: Assignment
requiredParameter = makeRequiredParameter <$> symbol Grammar.RequiredParameter <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeRequiredParameter loc (modifier, identifier, annotation, initializer) = makeTerm loc (Language.TypeScript.Syntax.RequiredParameter [modifier, annotation] (makeTerm loc (Statement.Assignment [] identifier initializer)))

restParameter :: Assignment
restParameter = makeRestParameter <$> symbol Grammar.RestParameter <*> children ((,) <$> identifier <*> (typeAnnotation' <|> emptyTerm))
  where makeRestParameter loc (identifier, annotation) = makeTerm loc (Language.TypeScript.Syntax.RestParameter [annotation] identifier)

optionalParameter :: Assignment
optionalParameter = makeOptionalParam <$> symbol Grammar.OptionalParameter <*> children ((,,,) <$> (accessibilityModifier' <|> emptyTerm) <*> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeOptionalParam loc (modifier, subject, annotation, initializer) = makeTerm loc (Language.TypeScript.Syntax.OptionalParameter [modifier, annotation] (makeTerm loc (Statement.Assignment [] subject initializer)))

internalModule :: Assignment
internalModule = makeTerm <$> symbol Grammar.InternalModule <*> children (Language.TypeScript.Syntax.InternalModule <$> (string <|> identifier <|> nestedIdentifier) <*> statements)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Declaration.Module <$> (string <|> identifier <|> nestedIdentifier) <*> ((symbol StatementBlock *> children (many statement)) <|> pure []))


statements :: HasCallStack => Assignment.Assignment [] Grammar [Term]
statements = symbol StatementBlock *> children (many statement)

arrowFunction :: Assignment
arrowFunction = makeArrowFun <$> symbol ArrowFunction <*> children ((,,) <$> emptyTerm <*> (((\a b c -> (a, [b], c)) <$> emptyTerm <*> identifier <*> emptyTerm) <|> callSignatureParts) <*> (expression <|> statementBlock))
  where makeArrowFun loc (identifier, (typeParams, params, returnTy), body) = makeTerm loc (Declaration.Function [ typeParams, returnTy ] identifier params body)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> parenthesizedExpression <*> statement <*> (statement <|> emptyTerm))

whileStatement :: Assignment
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> expression <*> statement)

forStatement :: Assignment
forStatement = makeTerm <$> symbol ForStatement <*> children (Statement.For <$> (variableDeclaration <|> expressionStatement' <|> emptyStatement) <*> (expressionStatement' <|> emptyStatement) <*> (expression <|> emptyTerm) <*> statement)

variableDeclaration :: Assignment
variableDeclaration = makeTerm <$> (symbol Grammar.VariableDeclaration <|> symbol LexicalDeclaration) <*> children (Language.TypeScript.Syntax.VariableDeclaration <$> many variableDeclarator)

variableDeclarator :: Assignment
variableDeclarator = makeVarDecl <$> symbol VariableDeclarator <*> children ((,,) <$> (identifier <|> destructuringPattern) <*> (typeAnnotation' <|> emptyTerm) <*> (expression <|> emptyTerm))
  where makeVarDecl loc (subject, annotations, value) = makeTerm loc (Statement.Assignment [annotations] subject value)

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children (expression <|> sequenceExpression)

switchStatement :: Assignment
switchStatement = makeTerm <$> symbol SwitchStatement <*> children (Statement.Match <$> parenthesizedExpression <*> switchBody)
  where
    switchBody =  symbol SwitchBody *> children (makeTerm <$> location <*> many switchCase)
    switchCase = makeTerm <$> (symbol SwitchCase <|> symbol SwitchDefault) <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some expression) <*> (makeTerm <$> location <*> many statement))

subscriptExpression :: Assignment
subscriptExpression = makeTerm <$> symbol SubscriptExpression <*> children (Expression.Subscript <$> expression <*> (pure <$> expression))

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> propertyName <*> expression)

callExpression :: Assignment
callExpression = makeCall <$> symbol CallExpression <*> children ((,,,) <$> (expression <|> super <|> function) <*> (typeArguments <|> pure []) <*> (arguments <|> (pure <$> templateString)) <*> emptyTerm)
  where makeCall loc (subject, typeArgs, args, body) = makeTerm loc (Expression.Call typeArgs subject args body)
        arguments = symbol Arguments *> children (many (expression <|> spreadElement))
        typeArguments = symbol Grammar.TypeArguments *> children (some ty)

tryStatement :: Assignment
tryStatement = makeTry <$> symbol TryStatement <*> children ((,,) <$> statementBlock <*> optional catchClause <*> optional finallyClause)
  where
    makeTry loc (statementBlock', catch, finally) = makeTerm loc (Statement.Try statementBlock' (catMaybes [catch, finally]))
    catchClause = makeTerm <$> symbol CatchClause <*> children (Statement.Catch <$> (identifier <|> emptyTerm) <*> statementBlock)
    finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> statementBlock)

binaryExpression  :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression expression
  [ (inj .) . Expression.Plus             <$ symbol AnonPlus
  , (inj .) . Expression.Minus            <$ symbol AnonMinus
  , (inj .) . Expression.Times            <$ symbol AnonStar
  , (inj .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inj .) . Expression.Modulo           <$ symbol AnonPercent
  , (inj .) . Expression.Member           <$ symbol AnonIn
  , (inj .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inj .) . Expression.Or               <$ symbol AnonPipePipe
  , (inj .) . Expression.BOr              <$ symbol AnonPipe
  , (inj .) . Expression.BXOr             <$ symbol AnonCaret
  , (inj .) . Expression.InstanceOf       <$ symbol AnonInstanceof
  , (inj .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual)
  , (inj .) . invert Expression.Equal     <$ (symbol AnonBangEqual <|> symbol AnonBangEqualEqual)
  , (inj .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inj .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inj .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inj .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inj .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inj .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ source <|> pure Syntax.Empty)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: HasCallStack
          => Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Data.Union.Union Syntax Term)]
          -> Assignment.Assignment [] Grammar (Data.Union.Union Syntax Term)
infixTerm = infixContext comment
