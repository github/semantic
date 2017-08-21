{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}

module Language.TypeScript.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Algorithm
import GHC.Generics
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.ByteString (ByteString)
import Data.Align.Generic
import Data.Maybe (fromMaybe)
import Data.Record
import Data.Syntax (emptyTerm, handleError, makeTerm)
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
  , Literal.Array
  , Literal.Boolean
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.String
  , Literal.TextElement
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.Else
  , Statement.Finally
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
  , Syntax.AccessibilityModifier
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Program
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
  , Language.TypeScript.Syntax.ClassHeritage
  , Language.TypeScript.Syntax.AbstractClass
  , Language.TypeScript.Syntax.ExtendsClause
  , Language.TypeScript.Syntax.ImplementsClause
  , Type.Visibility
  , []
  ]

type Term = Term.Term (Data.Union.Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term

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

data Function a = Function { functionTypeParameters :: !a, functionFormalParameters :: ![a], functionType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

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

data PredefinedType a = PredefinedType { predefinedType :: !a }
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

data NonNullExpression a = NonNullExpression { nonNullExpression :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 NonNullExpression where liftEq = genericLiftEq
instance Show1 NonNullExpression where liftShowsPrec = genericLiftShowsPrec

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

data ClassHeritage a = ClassHeritage { extendsClause :: !a, implementsClause :: !a }
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

data ExtendsClause a = ExtendsClause { extendsClauseTypes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec

data ImplementsClause a = ImplementsClause { implementsClauseTypes :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec

-- | Assignment from AST in Ruby’s grammar onto a program in TypeScript’s syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Syntax.Program <$> many expression)

expression :: Assignment
expression = handleError $
  comment
  <|> typeAssertion
  <|> asExpression
  <|> nonNullExpression'
  <|> importAlias'
  <|> internalModule
  <|> super
  <|> abstractClass
  <|> object
  <|> array
  <|> jsxElement
  <|> jsxSelfClosingElement
  <|> class'
  <|> anonymousClass
  <|> function
  <|> arrowFunction
  <|> generatorFunction
  <|> assignmentExpression
  <|> augmentedAssignmentExpression
  <|> awaitExpression
  <|> unaryExpression
  <|> binaryExpression
  <|> ternaryExpression
  <|> updateExpression
  <|> callExpression
  <|> memberExpression
  <|> newExpression
  <|> parenthesizedExpression
  <|> subscriptExpression
  <|> yieldExpression
  <|> thisExpression
  <|> number
  <|> string
  <|> templateString
  <|> regex
  <|> true
  <|> false
  <|> null'
  <|> undefined
  <|> identifier

anonymousClass :: Assignment
anonymousClass = makeTerm <$> symbol Grammar.AnonymousClass <*> children (Declaration.Class <$> emptyTerm <*> pure [] <*> superclass <*> classBodyStatements)
  where superclass = pure <$ symbol Grammar.ClassHeritage <*> children expression

abstractClass :: Assignment
abstractClass = makeTerm <$> symbol Grammar.AbstractClass <*> (Language.TypeScript.Syntax.AbstractClass <$> identifier <*> (typeParameters <|> emptyTerm) <*> (classHeritage <|> pure []) <*> classBodyStatements)
  where classHeritage = (\a b -> a : b : []) <$> (extendsClause' <|> emptyTerm) <*> implementsClause'

classHeritage' :: Assignment
classHeritage' = makeTerm <$> symbol Grammar.ClassHeritage <*> (Language.TypeScript.Syntax.ClassHeritage <$> (extendsClause' <|> emptyTerm) <*> implementsClause')

extendsClause' :: Assignment
extendsClause' = makeTerm <$> symbol Grammar.ExtendsClause <*> children (Language.TypeScript.Syntax.ExtendsClause <$> many ty)

implementsClause' :: Assignment
implementsClause' = makeTerm <$> symbol Grammar.ImplementsClause <*> children (Language.TypeScript.Syntax.ImplementsClause <$> many ty)

super :: Assignment
super = makeTerm <$> symbol Grammar.Super <*> (Language.TypeScript.Syntax.Super <$ source)

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol Grammar.TypeAssertion <*> (Language.TypeScript.Syntax.TypeAssertion <$> typeArguments' <*> expression)

asExpression :: Assignment
asExpression = makeTerm <$> symbol AsExpression <*> (Language.TypeScript.Syntax.Cast <$> expression <*> (ty <|> templateString))

templateString :: Assignment
templateString = makeTerm <$> symbol TemplateString <*> (Literal.String <$> many templateSubstitution)

templateSubstitution :: Assignment
templateSubstitution = symbol TemplateSubstitution *> children expression

nonNullExpression' :: Assignment
nonNullExpression' = makeTerm <$> symbol Grammar.NonNullExpression <*> (Language.TypeScript.Syntax.NonNullExpression <$> expression)

importAlias' :: Assignment
importAlias' = makeTerm <$> symbol Grammar.ImportAlias <*> (Language.TypeScript.Syntax.ImportAlias <$> identifier <*> (identifier <|> nestedIdentifier))

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
class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> expression <*> (many typeParameter' <|> pure []) (superclass <|> pure []) <*> classBodyStatements)
  where superclass = pure <$ symbol Grammar.ClassHeritage <*> children expression

object :: Assignment
object = makeTerm <$> symbol Object <*> children (Literal.Hash <$> many (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandReservedIdentifier))

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
jsxText = makeTerm <$> symbol Grammar.JsxText <*> children (Language.TypeScript.Syntax.JsxText <$> source)

jsxClosingElement' :: Assignment
jsxClosingElement' = makeTerm <$> symbol Grammar.JsxClosingElement <*> children (Language.TypeScript.Syntax.JsxClosingElement <$> identifier)

jsxAttribute :: Assignment
jsxAttribute = makeTerm <$> symbol Grammar.JsxAttribute <*> children (Language.TypeScript.Syntax.JsxAttribute <$> propertyIdentifier <*> (number <|> string <|> jsxExpression'))

parameter :: Assignment
parameter =
      requiredParameter
  <|> restParameter
  <|> optionalParameter

accessibilityModifier' :: Assignment
accessibilityModifier' = makeTerm <$> symbol AccessibilityModifier <*> children (Syntax.Identifier <$> source)

destructuringPattern :: Assignment
destructuringPattern = makeTerm <$> symbol ObjectPattern <*> (Literal.Hash <$> many (pair <|> spreadElement <|> methodDefinition <|> assignmentPattern <|> shorthandPropertyIdentifier))

spreadElement :: Assignment
spreadElement = symbol SpreadElement *> children expression

readonly' :: Assignment
readonly' = makeTerm <$> symbol Readonly <*> children (Syntax.Identifier <$> source)

methodDefinition :: Assignment
methodDefinition = makeMethod <$>
  symbol MethodDefinition
  <*> children ((,,,,,) <$> (fromMaybe <$> emptyTerm <*> optional accessibilityModifier') <*> (fromMaybe <$> emptyTerm <*> optional readonly') <*> emptyTerm <*> emptyTerm <*> callSignatureParts <*> emptyTerm)
  where
    makeMethod loc (modifier, readonly, receiver, propertyName', (typeParameters', params, ty'), statements) = makeTerm loc (Declaration.Method [modifier, readonly, typeParameters', ty'] receiver propertyName' params statements)

callSignatureParts :: HasCallStack => Assignment.Assignment (AST Grammar) Grammar (Term, [Term], Term)
callSignatureParts =  symbol Grammar.CallSignature *> children ((,,) <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

callSignature :: Assignment
callSignature =  makeTerm <$> symbol Grammar.CallSignature <*> children (Language.TypeScript.Syntax.CallSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

constructSignature :: Assignment
constructSignature = makeTerm <$> symbol Grammar.ConstructSignature <*> children (Language.TypeScript.Syntax.ConstructSignature <$> (fromMaybe <$> emptyTerm <*> optional typeParameters) <*> formalParameters <*> (fromMaybe <$> emptyTerm <*> optional typeAnnotation'))

indexSignature :: Assignment
indexSignature = makeTerm <$> symbol Grammar.IndexSignature <*> children (Language.TypeScript.Syntax.IndexSignature <$> (identifier <|> typeAnnotation'))

methodSignature :: Assignment
methodSignature = makeMethodSignature <$> symbol Grammar.MethodSignature <*> children ((,,,) <$> (fromMaybe <$> emptyTerm <*> optional accessibilityModifier') <*> (fromMaybe <$> emptyTerm <*> optional readonly') <*> propertyName <*> callSignatureParts)
  where makeMethodSignature loc (modifier, readonly, propertyName, (typeParams, params, annotation)) = makeTerm loc (Language.TypeScript.Syntax.MethodSignature [modifier, readonly, typeParams, annotation] propertyName params)

formalParameters :: HasCallStack => Assignment.Assignment (AST Grammar) Grammar [Term]
formalParameters = symbol FormalParameters *> children (many parameter)

typeParameters :: Assignment
typeParameters = makeTerm <$> symbol TypeParameters <*> children (Type.TypeParameters <$> many typeParameter')

typeAnnotation' :: Assignment
typeAnnotation' = makeTerm <$> symbol TypeAnnotation <*> children (Language.TypeScript.Syntax.Annotation <$> ty)

typeParameter' :: Assignment
typeParameter' = makeTerm <$> symbol Grammar.TypeParameter <*> children (Language.TypeScript.Syntax.TypeParameter <$> ty <*> (fromMaybe <$> emptyTerm <*> optional constraint))

constraint :: Assignment
constraint = makeTerm <$> symbol Grammar.Constraint <*> children (Language.TypeScript.Syntax.Constraint <$> ty)

function :: Assignment
function = makeFunction <$> symbol Grammar.Function <*> children ((,,) <$> identifier <*> callSignatureParts <*> statements)
  where makeFunction loc (id, (typeParams, params, annotation), statements) = makeTerm loc (Declaration.Function [typeParams, annotation] id, params)

ty :: Assignment
ty = primaryType <|> unionType <|> intersectionType <|> functionTy <|> constructorTy

primaryType = parenthesizedTy <|> predefinedTy <|> typeIdentifier <|> nestedTypeIdentifier <|> genericType <|> typePredicate <|> objectType <|> arrayTy <|> tupleType <|> flowMaybeTy <|> typeQuery <|> indexTypeQuery <|> thisType <|> existentialType <|> literalType

parenthesizedTy :: Assignment
parenthesizedTy = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Language.TypeScript.Syntax.ParenthesizedType <$> ty)

predefinedTy :: Assignment
predefinedTy = makeTerm <$> symbol Grammar.PredefinedType <*> children (Language.TypeScript.Syntax.PredefinedType <$> ty)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol Grammar.TypeIdentifier <*> children (Language.TypeScript.Syntax.TypeIdentifier <$> source)

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
objectType = makeTerm <$> symbol Grammar.ObjectType <*> children (Language.TypeScript.Syntax.ObjectType <$> some (exportStatement <|> propertySignature <|> callSignature <|> constructSignature <|> indexSignature <|> methodSignature))

arrayTy :: Assignment
arrayTy = makeTerm <$> symbol Grammar.ArrayType <*> children (Language.TypeScript.Syntax.ArrayType <$> ty)

flowMaybeTy :: Assignment
flowMaybeTy = makeTerm <$> symbol Grammar.FlowMaybeType <*> children (Language.TypeScript.Syntax.FlowMaybeType <$> primaryType)

typeQuery :: Assignment
typeQuery = makeTerm <$> symbol Grammar.TypeQuery <*> children (Language.TypeScript.Syntax.TypeQuery <$> (identifier <|> nestedIdentifier))

indexTypeQuery :: Assignment
indexTypeQuery = makeTerm <$> symbol Grammar.IndexTypeQuery <*> children (Language.TypeScript.Syntax.IndexTypeQuery <$> (identifier <|> nestedIdentifier))

thisType :: Assignment
thisType = makeTerm <$> symbol Grammar.ThisType <*> children (Language.TypeScript.Syntax.ThisType <$> source)

existentialType :: Assignment
existentialType = makeTerm <$> symbol Grammar.ExistentialType <*> (Language.TypeScript.Syntax.ExistentialType <$> source)

literalType :: Assignment
literalType = makeTerm <$> symbol Grammar.LiteralType <*> (Language.TypeScript.Syntax.LiteralType <$> (number <|> string <|> true <|> false))

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

classBodyStatements :: HasCallStack => Assignment.Assignment (AST Grammar) Grammar [Term]
classBodyStatements = symbol ClassBody *> children (many statement)

statement :: Assignment
statement =
  exportStatement
  <|> importStatement
  <|> debuggerStatement
  <|> expressionStatement'
  <|> declaration
  <|> statementBlock
  <|> ifStatement
  <|> switchStatement
  <|> forStatement
  <|> forInStatement
  <|> forOfStatement
  <|> whileStatement
  <|> doStatement
  <|> tryStatement
  <|> withStatement
  <|> breakStatement
  <|> continueStatement
  <|> returnStatement
  <|> throwStatement
  <|> emptyStatement
  <|> labeledStatement

importStatement :: Assignment
importStatement = makeTerm <$> symbol Grammar.ImportStatement <*> children (Language.TypeScript.Syntax.Import <$> (((\a b -> a : b : []) <$> importClause <*> fromClause) <|> (pure <$> (importRequireClause <|> string))))

debuggerStatement :: Assignment
debuggerStatement = makeTerm <$> symbol Grammar.DebuggerStatement <*> children (Language.TypeScript.Syntax.Debugger <$ source)

expressionStatement' :: Assignment
expressionStatement' = makeTerm <$> symbol Grammar.ExpressionStatement <*> children (Language.TypeScript.Syntax.ExpressionStatement <$> (expression <|> sequenceExpression))

declaration :: Assignment
declaration = exportStatement <|> importAlias' <|> function <|> internalModule <|> ambientFunction <|> generatorFunction <|> class' <|> module' <|> variableDeclaration <|> lexicalDeclaration <|> typeAliasDeclaration <|> enumDeclaration <|> interfaceDeclaration <|> ambientDeclaration

-- | Not sure if this use of <> is right.
exportStatement :: Assignment
exportStatement = makeTerm <$> symbol Grammar.ExportStatement <*> children (Language.TypeScript.Syntax.Export <$> ((<>) <$> fromClause <|> (exportClause <*> fromClause) <|> exportClause <|> declaration <|> expression <|> identifier <|> importAlias'))

propertySignature :: Assignment
propertySignature = makePropertySignature <$> symbol Grammar.PropertySignature <*> children ((,,,) <$> optional accessibilityModifier <*> optional readonly <*> propertyName <*> optional typeAnnotation')
  where makePropertySignature (modifier, readonly, propertyName, annotation) = Language.TypeScript.Syntax.PropertySignature [modifier, readonly, annotation] propertyName

propertyName :: Assignment
propertyName = makeTerm <$> symbol PropertyIdentifier <*> children ((Syntax.Identifier <$> source) <|> string <|> number)

assignmentPattern :: Assignment
assignmentPattern = makeTerm <$> symbol AssignmentPattern <*> children (Assignment <$> shorthandPropertyIdentifier <*> initializer)

shorthandPropertyIdentifier :: Assignment
shorthandPropertyIdentifier = makeTerm <$> symbol Grammar.ShorthandPropertyIdentifier <*> (Language.TypeScript.Syntax.ShorthandPropertyIdentifier <$> source)

requiredParameter :: Assignment
requiredParameter = makeVisibility <$> symbol RequiredParameter <*> children ((,,,) <$> optional accessibilityModifier' <*> (identifier <|> destructuringPattern) <*> optional typeAnnotation' <*> optional initializer)
  where makeVisibility loc (modifier, identifier, annotation, initializer) = maybe method' (makeTerm . Visibility method') modifier
        param' identifier initializer = makeTerm loc (fmap Declaration.RequiredParameter . term')
          where term' = maybe identifier (Statement.Assignment <$> identifier <*>) initializer

restParameter :: Assignment
restParameter = makeTerm <$> symbol RestParameter <*> children ((,) <$> identifier <*> optional typeAnnotation')

optionalParameter :: Assignment
optionalParameter = makeTerm <$> symbol OptionalParameter <*> children ((,,,) <$> optional accessibilityModifier' <*> (identifier <|> destructuringPattern) <*> optional typeAnnotation' <*> optional initializer)

internalModule :: Assignment
internalModule = makeTerm <$> symbol Grammar.InternalModule <*> children (Language.TypeScript.Syntax.InternalModule <$> (string <|> identifier <|> nestedIdentifier) <*> (fromMaybe <$> emptyTerm <*> optional statementBlock))

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Declaration.Module <$> (string <|> identifier <|> nestedIdentifier) <*> (fromMaybe <$> [] <*> optional (symbol StatementBlock *> children (many statement))))

lambda :: Assignment
lambda = symbol Lambda >>= \ loc -> children $ do
  name <- makeTerm loc <$> (Syntax.Identifier <$> source)
  params <- (symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure []
  body <- expressions
  pure $ makeTerm loc (Declaration.Function name params body)

block :: Assignment
block =  makeTerm <$> symbol DoBlock <*> children (Declaration.Function <$> emptyTerm <*> params <*> expressions)
     <|> makeTerm <$> symbol Block <*> children (Declaration.Function <$> emptyTerm <*> params <*> expressions)
  where params = (symbol BlockParameters) *> children (many parameter) <|> pure []

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

ifStatement :: Assignment
ifStatement = ifElseif If
  where ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> parenthesizedExpression <*> statement <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> else')))

else' :: Assignment
else' = makeTerm <$> symbol Else <*> children (many expression)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> vars <*> expression <*> expressions)
  where vars = makeTerm <$> location <*> some expression

case' :: Assignment
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> expression <*> when')
  where
    when' =  makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern) <*> (when' <|> else' <|> expressions))
    pattern = symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression)

subscript :: Assignment
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many argument)

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

argument :: Assignment
argument =
      mk SplatArgument
  <|> mk HashSplatArgument
  <|> mk BlockArgument
  <|> pair
  <|> expression
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

methodCall :: Assignment
methodCall = makeTerm <$> symbol MethodCall <*> children (Expression.Call <$> expression <*> args <*> (block <|> emptyTerm))
  where
    args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many argument) <|> pure []

call :: Assignment
call = makeTerm <$> symbol Call <*> children (Expression.MemberAccess <$> expression <*> expression)

rescue :: Assignment
rescue =  rescue'
      <|> makeTerm <$> symbol RescueModifier <*> children (Statement.Try <$> expression <*> many (makeTerm <$> location <*> (Statement.Catch <$> expression <*> emptyTerm)))
      <|> makeTerm <$> symbol Ensure <*> children (Statement.Finally <$> expressions)
      <|> makeTerm <$> symbol Else <*> children (Statement.Else <$> emptyTerm <*> expressions)
  where
    rescue' = makeTerm <$> symbol Rescue <*> children (Statement.Catch <$> exceptions <*> (rescue' <|> expressions))
    exceptions = makeTerm <$> location <*> many ex
    ex =  makeTerm <$> symbol Exceptions <*> children (many expression)
      <|> makeTerm <$> symbol ExceptionVariable <*> children (many expression)

begin :: Assignment
begin = makeTerm <$> symbol Begin <*> children (Statement.Try <$> expressions <*> many rescue)

assignment' :: Assignment
assignment'
   =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> lhs <*> rhs)
  <|> makeTerm <$> symbol OperatorAssignment <*> children (lhs >>= \ var -> Statement.Assignment var <$>
         (makeTerm <$> symbol AnonPlusEqual               <*> (Expression.Plus var      <$> expression)
      <|> makeTerm <$> symbol AnonMinusEqual              <*> (Expression.Minus var     <$> expression)
      <|> makeTerm <$> symbol AnonStarEqual               <*> (Expression.Times var     <$> expression)
      <|> makeTerm <$> symbol AnonStarStarEqual           <*> (Expression.Power var     <$> expression)
      <|> makeTerm <$> symbol AnonSlashEqual              <*> (Expression.DividedBy var <$> expression)
      <|> makeTerm <$> symbol AnonPipePipeEqual           <*> (Expression.And var       <$> expression)
      <|> makeTerm <$> symbol AnonPipeEqual               <*> (Expression.BOr var       <$> expression)
      <|> makeTerm <$> symbol AnonAmpersandAmpersandEqual <*> (Expression.And var       <$> expression)
      <|> makeTerm <$> symbol AnonAmpersandEqual          <*> (Expression.BAnd var      <$> expression)
      <|> makeTerm <$> symbol AnonPercentEqual            <*> (Expression.Modulo var    <$> expression)
      <|> makeTerm <$> symbol AnonRAngleRAngleEqual       <*> (Expression.RShift var    <$> expression)
      <|> makeTerm <$> symbol AnonLAngleLAngleEqual       <*> (Expression.LShift var    <$> expression)
      <|> makeTerm <$> symbol AnonCaretEqual              <*> (Expression.BXOr var      <$> expression)))
  where
    lhs = makeTerm <$> symbol LeftAssignmentList <*> children (many expr) <|> expr
    rhs = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr =
          makeTerm <$> symbol RestAssignment <*> (Syntax.Identifier <$> source)
      <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
      <|> argument

unary :: Assignment
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call <$> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier <$> source)) <*> some expression <*> emptyTerm)
  <|> children ( symbol AnonPlus *> expression )
  <|> makeTerm location . Expression.Negate <$> children expression -- Unary minus (e.g. `-a`). HiddenUnaryMinus nodes are hidden, so we can't match on the symbol.

binary  :: Assignment
binary = symbol Binary >>= \ loc -> children $ expression >>= \ lexpression -> go loc lexpression
  where
    go loc lexpression
       =  mk AnonAnd Expression.And
      <|> mk AnonAmpersandAmpersand Expression.And
      <|> mk AnonOr Expression.Or
      <|> mk AnonPipePipe Expression.Or
      <|> mk AnonLAngleLAngle Expression.LShift
      <|> mk AnonRAngleRAngle Expression.RShift
      <|> mk AnonEqualEqual Expression.Equal
      <|> mkNot AnonBangEqual Expression.Equal
       -- TODO: Distinguish `===` from `==` ?
      <|> mk AnonEqualEqualEqual Expression.Equal
      <|> mk AnonLAngleEqualRAngle Expression.Comparison
      -- TODO: Distinuish `=~` and `!~` ?
      <|> mk AnonEqualTilde Expression.Equal
      <|> mkNot AnonBangTilde Expression.Equal
      <|> mk AnonLAngle Expression.LessThan
      <|> mk AnonLAngleEqual Expression.LessThanEqual
      <|> mk AnonRAngle Expression.GreaterThan
      <|> mk AnonRAngleEqual Expression.GreaterThanEqual
      <|> mk AnonAmpersand Expression.BAnd
      <|> mk AnonCaret Expression.BXOr
      <|> mk AnonPipe Expression.BOr
      -- TODO: binary minus (hidden node). Doesn't work b/c we can't match hidden nodes (they aren't in the tree).
      -- <|> mk HiddenBinaryMinus Expression.Minus
      <|> mk AnonPlus Expression.Plus
      -- TODO: binary star (hidden node)
      <|> mk AnonSlash Expression.DividedBy
      <|> mk AnonPercent Expression.Modulo
      <|> mk AnonStarStar Expression.Power
      where mk s constr = makeTerm loc <$> (symbol s *> (constr lexpression <$> expression))
            mkNot s constr = makeTerm loc <$ symbol s <*> (Expression.Not <$> (makeTerm <$> location <*> (constr lexpression <$> expression)))

conditional :: Assignment
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> expression <*> expression <*> expression)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ source <|> pure Syntax.Empty)


-- Helper functions
invert :: (Expression.Boolean :< fs, HasCallStack) => Assignment.Assignment ast grammar (Term.Term (Data.Union.Union fs) (Record Location)) -> Assignment.Assignment ast grammar (Term.Term (Data.Union.Union fs) (Record Location))
invert term = makeTerm <$> location <*> fmap Expression.Not term
