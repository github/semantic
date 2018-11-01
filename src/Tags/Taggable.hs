{-# LANGUAGE AllowAmbiguousTypes, GADTs, ConstraintKinds, KindSignatures, LambdaCase, RankNTypes, TypeFamilies, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Taggable
( Tagger
, Token(..)
, Taggable(..)
, IsTaggable
, HasTextElement
, tagging
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash, project)

import           Analysis.ConstructorName
import           Control.Matching hiding (target)
import           Data.Abstract.Declarations
import           Data.Abstract.Name
import           Data.Blob
import           Data.Language
import           Data.Location
import           Data.Machine as Machine
import           Data.Range
import           Data.Term
import           Data.Text hiding (empty)

import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Language.Go.Syntax as Go
import qualified Language.Go.Type as Go
import qualified Language.Haskell.Syntax as Haskell
import qualified Language.Java.Syntax as Java
import qualified Language.Markdown.Syntax as Markdown
import qualified Language.PHP.Syntax as PHP
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby
import qualified Language.TypeScript.Syntax as TypeScript



 -- TODO: Move to src/Data
data Token
  = Enter Text (Maybe Range)
  | Exit Text (Maybe Range)
  | Identifier Text (Maybe Range)
  deriving (Eq, Show)

data Tagger a where
  Pure :: a -> Tagger a
  Bind :: Tagger a -> (a -> Tagger b) -> Tagger b
  Tell :: Token -> Tagger ()
  Get :: Tagger InternalState
  Put :: InternalState -> Tagger ()

compile :: InternalState -> Tagger a -> Machine.Plan k Token (InternalState, a)
compile p = \case
  Pure a   -> pure (p, a)
  Bind a f -> compile p a >>= (\(new, v) -> compile new (f v))
  Tell t   -> Machine.yield t $> (p, ())
  Get      -> pure (p, p)
  Put p'   -> pure (p', ())

instance Functor Tagger where fmap = liftA
instance Applicative Tagger where
  pure  = Pure
  (<*>) = ap
instance Monad Tagger where (>>=) = Bind

enter, exit :: String -> Maybe Range -> Tagger ()
enter c = Tell . Enter (pack c)
exit c = Tell . Exit (pack c)

emitIden :: Maybe Range -> Name -> Tagger ()
emitIden range name = Tell (Identifier (formatName name) range)

data InternalState
  = InternalState
  { _location  :: Location
  , _language :: Language
  }

 -- InternalState handling
asks :: (InternalState -> a) -> Tagger a
asks f = f <$> Get

-- modify :: (InternalState -> InternalState) -> Tagger ()
-- modify f = Get >>= \x -> Put . f $! x

class (Show1 constr, Traversable constr) => Taggable constr where
  docsLiteral ::
    ( Functor syntax
    , Foldable syntax
    , HasTextElement syntax
    )
    => Language -> constr (Term syntax Location) -> Maybe Range
  docsLiteral _ _ = Nothing

  snippet :: Location -> constr (Term syntax Location) -> Maybe Range
  snippet _ _ = Nothing

-- withLocation :: Annotated t Location => t -> Tagger a -> Tagger a
-- withLocation t act = do
--   old <- asks location
--   modify (\x -> x { location = annotation t })
--   act <* modify (\x -> x { location = old })

type IsTaggable syntax =
  ( Functor syntax
  , Foldable syntax
  , Traversable syntax
  , Show1 syntax
  , Taggable syntax
  , ConstructorName syntax
  , Declarations1 syntax
  , HasTextElement syntax
  )

tagging :: (IsTaggable syntax)
  => Blob
  -> Term syntax Location
  -> Machine.Source Token
tagging Blob{..} term = pipe
  where pipe  = Machine.construct . fmap snd $ compile state go
        state = InternalState (termAnnotation term) blobLanguage
        go    = foldSubterms descend term

descend :: forall constr syntax.
  ( Taggable constr
  , ConstructorName constr
  , Declarations (constr (Term syntax Location))
  , Functor syntax
  , Foldable syntax
  , HasTextElement syntax
  )
  => SubtermAlgebra constr (Term syntax Location) (Tagger ())
descend t = do
  (InternalState loc lang) <- asks id
  let term = fmap subterm t
  let snippetRange = snippet loc term
  let litRange = docsLiteral lang term

  enter (constructorName term) snippetRange
  maybe (pure ()) (emitIden litRange) (declaredName term)
  traverse_ subtermRef t
  exit (constructorName term) snippetRange

subtractLocation :: Location -> Location -> Range
subtractLocation a b = subtractRange (locationByteRange a) (locationByteRange b)

-- Instances

instance ( Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Taggable fs) => Taggable (Sum fs) where
  docsLiteral a = apply @Taggable (docsLiteral a)
  snippet x = apply @Taggable (snippet x)

instance (Taggable a) => Taggable (TermF a Location) where
  docsLiteral l t = docsLiteral l (termFOut t)
  snippet _ t = snippet (termFAnnotation t) (termFOut t)

instance Taggable Syntax.Context where
  snippet ann (Syntax.Context _ (Term (In sub _))) = Just $ subtractLocation ann sub

instance Taggable Declaration.Function where
  docsLiteral Python (Declaration.Function _ _ _ (Term (In _ bodyF)))
    | (Term (In exprAnn exprF):_ ) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise        = Nothing
  docsLiteral _ _ = Nothing
  snippet ann (Declaration.Function _ _ _ (Term (In body _))) = Just $ subtractLocation ann body

instance Taggable Declaration.Method where
  docsLiteral _ _ = Nothing
  snippet ann (Declaration.Method _ _ _ _ (Term (In body _))) = Just $ subtractLocation ann body

-- TODO: Fill these out
instance Taggable Ruby.Class
instance Taggable Ruby.Module


instance Taggable []
instance Taggable Comment.Comment
instance Taggable Comment.HashBang

instance Taggable Expression.And
instance Taggable Expression.Await
instance Taggable Expression.BAnd
instance Taggable Expression.BOr
instance Taggable Expression.BXOr
instance Taggable Expression.Call
instance Taggable Expression.Cast
instance Taggable Expression.Comparison
instance Taggable Expression.Complement
instance Taggable Expression.Delete
instance Taggable Expression.DividedBy
instance Taggable Expression.Enumeration
instance Taggable Expression.Equal
instance Taggable Expression.FloorDivision
instance Taggable Expression.GreaterThan
instance Taggable Expression.GreaterThanEqual
instance Taggable Expression.InstanceOf
instance Taggable Expression.LessThan
instance Taggable Expression.LessThanEqual
instance Taggable Expression.LShift
instance Taggable Expression.Matches
instance Taggable Expression.Member
instance Taggable Expression.MemberAccess
instance Taggable Expression.Minus
instance Taggable Expression.Modulo
instance Taggable Expression.Negate
instance Taggable Expression.New
instance Taggable Expression.NonNullExpression
instance Taggable Expression.Not
instance Taggable Expression.NotMatches
instance Taggable Expression.Or
instance Taggable Expression.Plus
instance Taggable Expression.Power
instance Taggable Expression.RShift
instance Taggable Expression.ScopeResolution
instance Taggable Expression.SequenceExpression
instance Taggable Expression.StrictEqual
instance Taggable Expression.Subscript
instance Taggable Expression.Super
instance Taggable Expression.This
instance Taggable Expression.Times
instance Taggable Expression.Typeof
instance Taggable Expression.UnsignedRShift
instance Taggable Expression.Void
instance Taggable Expression.XOr

instance Taggable Literal.Boolean
instance Taggable Literal.Integer
instance Taggable Literal.Float
instance Taggable Literal.Rational
instance Taggable Literal.Complex
instance Taggable Literal.String
instance Taggable Literal.Character
instance Taggable Literal.InterpolationElement
instance Taggable Literal.TextElement
instance Taggable Literal.EscapeSequence
instance Taggable Literal.Symbol
instance Taggable Literal.SymbolElement
instance Taggable Literal.Regex
instance Taggable Literal.Array
instance Taggable Literal.Hash
instance Taggable Literal.Tuple
instance Taggable Literal.Set
instance Taggable Literal.Pointer
instance Taggable Literal.Reference
instance Taggable Literal.Null
instance Taggable Literal.KeyValue

instance Taggable Statement.Assignment
instance Taggable Statement.Break
instance Taggable Statement.Catch
instance Taggable Statement.Continue
instance Taggable Statement.DoWhile
instance Taggable Statement.Else
instance Taggable Statement.Finally
instance Taggable Statement.For
instance Taggable Statement.ForEach
instance Taggable Statement.Goto
instance Taggable Statement.If
instance Taggable Statement.Let
instance Taggable Statement.Match
instance Taggable Statement.NoOp
instance Taggable Statement.Pattern
instance Taggable Statement.PostDecrement
instance Taggable Statement.PostIncrement
instance Taggable Statement.PreDecrement
instance Taggable Statement.PreIncrement
instance Taggable Statement.Retry
instance Taggable Statement.Return
instance Taggable Statement.ScopeEntry
instance Taggable Statement.ScopeExit
instance Taggable Statement.Statements
instance Taggable Statement.Throw
instance Taggable Statement.Try
instance Taggable Statement.While
instance Taggable Statement.Yield

instance Taggable Syntax.Empty
instance Taggable Syntax.Error
instance Taggable Syntax.Identifier
instance Taggable Syntax.AccessibilityModifier

instance Taggable Type.Annotation
instance Taggable Type.Array
instance Taggable Type.Bool
instance Taggable Type.Double
instance Taggable Type.Float
instance Taggable Type.Function
instance Taggable Type.Int
instance Taggable Type.Interface
instance Taggable Type.Map
instance Taggable Type.Parenthesized
instance Taggable Type.Pointer
instance Taggable Type.Product
instance Taggable Type.Readonly
instance Taggable Type.Slice
instance Taggable Type.TypeParameters
instance Taggable Type.Void

instance Taggable Declaration.Class
instance Taggable Declaration.Comprehension
instance Taggable Declaration.Constructor
instance Taggable Declaration.Datatype
instance Taggable Declaration.Decorator
instance Taggable Declaration.InterfaceDeclaration
instance Taggable Declaration.MethodSignature
instance Taggable Declaration.OptionalParameter
instance Taggable Declaration.PublicFieldDefinition
instance Taggable Declaration.RequiredParameter
instance Taggable Declaration.Type
instance Taggable Declaration.TypeAlias
instance Taggable Declaration.Variable
instance Taggable Declaration.VariableDeclaration

instance Taggable Directive.File
instance Taggable Directive.Line

instance Taggable Haskell.UnitConstructor
instance Taggable Haskell.ListConstructor
instance Taggable Haskell.FunctionConstructor
instance Taggable Haskell.RecordDataConstructor
instance Taggable Haskell.AllConstructors
instance Taggable Haskell.GADTConstructor
instance Taggable Haskell.LabeledConstruction
instance Taggable Haskell.InfixDataConstructor
instance Taggable Haskell.TupleConstructor
instance Taggable Haskell.TypeConstructorExport
instance Taggable Haskell.KindParenthesizedConstructor
instance Taggable Haskell.ConstructorSymbol
instance Taggable Haskell.Module
instance Taggable Haskell.Field
instance Taggable Haskell.GADT
instance Taggable Haskell.InfixOperatorPattern
instance Taggable Haskell.NewType
instance Taggable Haskell.ImportDeclaration
instance Taggable Haskell.QualifiedImportDeclaration
instance Taggable Haskell.ImportAlias
instance Taggable Haskell.App
instance Taggable Haskell.InfixOperatorApp
instance Taggable Haskell.ListComprehension
instance Taggable Haskell.Generator
instance Taggable Haskell.ArithmeticSequence
instance Taggable Haskell.RightOperatorSection
instance Taggable Haskell.LeftOperatorSection
instance Taggable Haskell.BindPattern
instance Taggable Haskell.Lambda
instance Taggable Haskell.FixityAlt
instance Taggable Haskell.RecordWildCards
instance Taggable Haskell.Wildcard
instance Taggable Haskell.Let
instance Taggable Haskell.FieldBind
instance Taggable Haskell.Pragma
instance Taggable Haskell.Deriving
instance Taggable Haskell.ContextAlt
instance Taggable Haskell.Class
instance Taggable Haskell.Export
instance Taggable Haskell.ModuleExport
instance Taggable Haskell.QuotedName
instance Taggable Haskell.ScopedTypeVariables
instance Taggable Haskell.DefaultDeclaration
instance Taggable Haskell.VariableOperator
instance Taggable Haskell.ConstructorOperator
instance Taggable Haskell.TypeOperator
instance Taggable Haskell.PromotedTypeOperator
instance Taggable Haskell.VariableSymbol
instance Taggable Haskell.Import
instance Taggable Haskell.HiddenImport
instance Taggable Haskell.TypeApp
instance Taggable Haskell.TupleExpression
instance Taggable Haskell.TuplePattern
instance Taggable Haskell.ConstructorPattern
instance Taggable Haskell.Do
instance Taggable Haskell.PrefixNegation
instance Taggable Haskell.CPPDirective
instance Taggable Haskell.NamedFieldPun
instance Taggable Haskell.NegativeLiteral
instance Taggable Haskell.LambdaCase
instance Taggable Haskell.LabeledUpdate
instance Taggable Haskell.QualifiedTypeClassIdentifier
instance Taggable Haskell.QualifiedTypeConstructorIdentifier
instance Taggable Haskell.QualifiedConstructorIdentifier
instance Taggable Haskell.QualifiedInfixVariableIdentifier
instance Taggable Haskell.QualifiedModuleIdentifier
instance Taggable Haskell.QualifiedVariableIdentifier
instance Taggable Haskell.TypeVariableIdentifier
instance Taggable Haskell.TypeConstructorIdentifier
instance Taggable Haskell.ModuleIdentifier
instance Taggable Haskell.ConstructorIdentifier
instance Taggable Haskell.ImplicitParameterIdentifier
instance Taggable Haskell.InfixConstructorIdentifier
instance Taggable Haskell.InfixVariableIdentifier
instance Taggable Haskell.TypeClassIdentifier
instance Taggable Haskell.VariableIdentifier
instance Taggable Haskell.PrimitiveConstructorIdentifier
instance Taggable Haskell.PrimitiveVariableIdentifier
instance Taggable Haskell.AsPattern
instance Taggable Haskell.FieldPattern
instance Taggable Haskell.ViewPattern
instance Taggable Haskell.PatternGuard
instance Taggable Haskell.StrictPattern
instance Taggable Haskell.ListPattern
instance Taggable Haskell.TypePattern
instance Taggable Haskell.IrrefutablePattern
instance Taggable Haskell.CaseGuardPattern
instance Taggable Haskell.FunctionGuardPattern
instance Taggable Haskell.LabeledPattern
instance Taggable Haskell.Guard
instance Taggable Haskell.QuasiQuotation
instance Taggable Haskell.QuasiQuotationPattern
instance Taggable Haskell.QuasiQuotationType
instance Taggable Haskell.QuasiQuotationDeclaration
instance Taggable Haskell.QuasiQuotationExpression
instance Taggable Haskell.QuasiQuotationExpressionBody
instance Taggable Haskell.QuasiQuotationQuoter
instance Taggable Haskell.Splice
instance Taggable Haskell.StrictType
instance Taggable Haskell.Type
instance Taggable Haskell.TypeSynonym
instance Taggable Haskell.AnnotatedTypeVariable
instance Taggable Haskell.StandaloneDerivingInstance
instance Taggable Haskell.FunctionType
instance Taggable Haskell.TypeSignature
instance Taggable Haskell.ExpressionTypeSignature
instance Taggable Haskell.KindFunctionType
instance Taggable Haskell.Star
instance Taggable Haskell.EqualityConstraint
instance Taggable Haskell.TypeInstance
instance Taggable Haskell.TypeClassInstance
instance Taggable Haskell.TypeClass
instance Taggable Haskell.DefaultSignature
instance Taggable Haskell.TypeFamily
instance Taggable Haskell.StrictTypeVariable
instance Taggable Haskell.KindSignature
instance Taggable Haskell.Kind
instance Taggable Haskell.KindListType
instance Taggable Haskell.Instance
instance Taggable Haskell.KindTupleType
instance Taggable Haskell.FunctionalDependency


instance Taggable Java.Import
instance Taggable Java.Package
instance Taggable Java.CatchType
instance Taggable Java.SpreadParameter
instance Taggable Java.StaticInitializer
instance Taggable Java.LambdaBody
instance Taggable Java.ClassBody
instance Taggable Java.ClassLiteral
instance Taggable Java.DefaultValue
instance Taggable Java.Module
instance Taggable Java.EnumDeclaration
instance Taggable Java.Variable
instance Taggable Java.Synchronized
instance Taggable Java.New
instance Taggable Java.Asterisk
instance Taggable Java.Constructor
instance Taggable Java.TypeParameter
instance Taggable Java.Annotation
instance Taggable Java.AnnotationField
instance Taggable Java.GenericType
instance Taggable Java.AnnotatedType
instance Taggable Java.TypeWithModifiers
instance Taggable Java.Wildcard
instance Taggable Java.WildcardBounds
instance Taggable Java.MethodReference
instance Taggable Java.NewKeyword
instance Taggable Java.Lambda
instance Taggable Java.ArrayCreationExpression
instance Taggable Java.DimsExpr
instance Taggable Java.TryWithResources
instance Taggable Java.AssertStatement
instance Taggable Java.AnnotationTypeElement

instance Taggable Python.Ellipsis
instance Taggable Python.FutureImport
instance Taggable Python.Import
instance Taggable Python.QualifiedAliasedImport
instance Taggable Python.QualifiedImport
instance Taggable Python.Redirect

instance Taggable Go.BidirectionalChannel
instance Taggable Go.ReceiveChannel
instance Taggable Go.SendChannel
instance Taggable Go.Import
instance Taggable Go.QualifiedImport
instance Taggable Go.SideEffectImport
instance Taggable Go.Composite
instance Taggable Go.Label
instance Taggable Go.Send
instance Taggable Go.Slice
instance Taggable Go.TypeSwitch
instance Taggable Go.Receive
instance Taggable Go.Field
instance Taggable Go.Package
instance Taggable Go.TypeAssertion
instance Taggable Go.TypeConversion
instance Taggable Go.Variadic
instance Taggable Go.DefaultPattern
instance Taggable Go.Defer
instance Taggable Go.Go
instance Taggable Go.Rune
instance Taggable Go.Select
instance Taggable Go.TypeSwitchGuard
instance Taggable Go.ReceiveOperator

instance Taggable Markdown.Document
instance Taggable Markdown.Paragraph
instance Taggable Markdown.UnorderedList
instance Taggable Markdown.OrderedList
instance Taggable Markdown.BlockQuote
instance Taggable Markdown.HTMLBlock
instance Taggable Markdown.Table
instance Taggable Markdown.TableRow
instance Taggable Markdown.TableCell
instance Taggable Markdown.Strong
instance Taggable Markdown.Emphasis
instance Taggable Markdown.Text
instance Taggable Markdown.Strikethrough
instance Taggable Markdown.Heading
instance Taggable Markdown.ThematicBreak
instance Taggable Markdown.Link
instance Taggable Markdown.Image
instance Taggable Markdown.Code
instance Taggable Markdown.LineBreak

instance Taggable PHP.Text
instance Taggable PHP.VariableName
instance Taggable PHP.Require
instance Taggable PHP.RequireOnce
instance Taggable PHP.Include
instance Taggable PHP.IncludeOnce
instance Taggable PHP.ArrayElement
instance Taggable PHP.GlobalDeclaration
instance Taggable PHP.SimpleVariable
instance Taggable PHP.CastType
instance Taggable PHP.ErrorControl
instance Taggable PHP.Clone
instance Taggable PHP.ShellCommand
instance Taggable PHP.Update
instance Taggable PHP.NewVariable
instance Taggable PHP.RelativeScope
instance Taggable PHP.NamespaceName
instance Taggable PHP.ConstDeclaration
instance Taggable PHP.ClassInterfaceClause
instance Taggable PHP.ClassBaseClause
instance Taggable PHP.UseClause
instance Taggable PHP.ReturnType
instance Taggable PHP.TypeDeclaration
instance Taggable PHP.BaseTypeDeclaration
instance Taggable PHP.ScalarType
instance Taggable PHP.EmptyIntrinsic
instance Taggable PHP.ExitIntrinsic
instance Taggable PHP.IssetIntrinsic
instance Taggable PHP.EvalIntrinsic
instance Taggable PHP.PrintIntrinsic
instance Taggable PHP.NamespaceAliasingClause
instance Taggable PHP.NamespaceUseDeclaration
instance Taggable PHP.NamespaceUseClause
instance Taggable PHP.NamespaceUseGroupClause
instance Taggable PHP.TraitUseSpecification
instance Taggable PHP.Static
instance Taggable PHP.ClassModifier
instance Taggable PHP.InterfaceBaseClause
instance Taggable PHP.Echo
instance Taggable PHP.Unset
instance Taggable PHP.DeclareDirective
instance Taggable PHP.LabeledStatement
instance Taggable PHP.QualifiedName
instance Taggable PHP.ClassConstDeclaration
instance Taggable PHP.Namespace
instance Taggable PHP.TraitDeclaration
instance Taggable PHP.AliasAs
instance Taggable PHP.InsteadOf
instance Taggable PHP.TraitUseClause
instance Taggable PHP.DestructorDeclaration
instance Taggable PHP.ConstructorDeclaration
instance Taggable PHP.PropertyDeclaration
instance Taggable PHP.PropertyModifier
instance Taggable PHP.InterfaceDeclaration
instance Taggable PHP.Declare

instance Taggable Ruby.Send
instance Taggable Ruby.Require
instance Taggable Ruby.Load
instance Taggable Ruby.LowPrecedenceAnd
instance Taggable Ruby.LowPrecedenceOr

instance Taggable TypeScript.JavaScriptRequire
instance Taggable TypeScript.Debugger
instance Taggable TypeScript.Super
instance Taggable TypeScript.Undefined
instance Taggable TypeScript.With
instance Taggable TypeScript.JsxElement
instance Taggable TypeScript.JsxOpeningElement
instance Taggable TypeScript.JsxSelfClosingElement
instance Taggable TypeScript.JsxAttribute
instance Taggable TypeScript.OptionalParameter
instance Taggable TypeScript.RequiredParameter
instance Taggable TypeScript.RestParameter
instance Taggable TypeScript.JsxNamespaceName
instance Taggable TypeScript.JsxText
instance Taggable TypeScript.JsxExpression
instance Taggable TypeScript.JsxClosingElement
instance Taggable TypeScript.ImplementsClause
instance Taggable TypeScript.JsxFragment
instance Taggable TypeScript.Import
instance Taggable TypeScript.QualifiedAliasedImport
instance Taggable TypeScript.QualifiedExportFrom
instance Taggable TypeScript.LookupType
instance Taggable TypeScript.Union
instance Taggable TypeScript.Intersection
instance Taggable TypeScript.FunctionType
instance Taggable TypeScript.AmbientFunction
instance Taggable TypeScript.ImportRequireClause
instance Taggable TypeScript.Constructor
instance Taggable TypeScript.TypeParameter
instance Taggable TypeScript.TypeAssertion
instance Taggable TypeScript.NestedIdentifier
instance Taggable TypeScript.NestedTypeIdentifier
instance Taggable TypeScript.GenericType
instance Taggable TypeScript.TypePredicate
instance Taggable TypeScript.EnumDeclaration
instance Taggable TypeScript.PropertySignature
instance Taggable TypeScript.CallSignature
instance Taggable TypeScript.ConstructSignature
instance Taggable TypeScript.IndexSignature
instance Taggable TypeScript.AbstractMethodSignature
instance Taggable TypeScript.ForOf
instance Taggable TypeScript.LabeledStatement
instance Taggable TypeScript.Module
instance Taggable TypeScript.InternalModule
instance Taggable TypeScript.ImportAlias
instance Taggable TypeScript.ClassHeritage
instance Taggable TypeScript.AbstractClass
instance Taggable TypeScript.SideEffectImport
instance Taggable TypeScript.QualifiedExport
instance Taggable TypeScript.DefaultExport
instance Taggable TypeScript.ShorthandPropertyIdentifier
instance Taggable TypeScript.ImportClause
instance Taggable TypeScript.Tuple
instance Taggable TypeScript.Annotation
instance Taggable TypeScript.Decorator
instance Taggable TypeScript.ComputedPropertyName
instance Taggable TypeScript.Constraint
instance Taggable TypeScript.DefaultType
instance Taggable TypeScript.ParenthesizedType
instance Taggable TypeScript.PredefinedType
instance Taggable TypeScript.TypeIdentifier
instance Taggable TypeScript.ObjectType
instance Taggable TypeScript.AmbientDeclaration
instance Taggable TypeScript.ExtendsClause
instance Taggable TypeScript.ArrayType
instance Taggable TypeScript.FlowMaybeType
instance Taggable TypeScript.TypeQuery
instance Taggable TypeScript.IndexTypeQuery
instance Taggable TypeScript.TypeArguments
instance Taggable TypeScript.ThisType
instance Taggable TypeScript.ExistentialType
instance Taggable TypeScript.LiteralType
instance Taggable TypeScript.Update



class HasTextElement syntax where
  isTextElement :: syntax a -> Bool

instance (TextElementStrategy syntax ~ strategy, HasTextElementWithStrategy strategy syntax) => HasTextElement syntax where
  isTextElement = isTextElementWithStrategy (Proxy :: Proxy strategy)

class CustomHasTextElement syntax where
  customIsTextElement :: syntax a -> Bool

instance CustomHasTextElement Literal.TextElement where
  customIsTextElement _ = True

instance Apply HasTextElement fs => CustomHasTextElement (Sum fs) where
  customIsTextElement = apply @HasTextElement isTextElement

data Strategy = Default | Custom

class HasTextElementWithStrategy (strategy :: Strategy) syntax where
  isTextElementWithStrategy :: proxy strategy -> syntax a -> Bool

type family TextElementStrategy syntax where
  TextElementStrategy Literal.TextElement = 'Custom
  TextElementStrategy (Sum fs) = 'Custom
  TextElementStrategy a = 'Default

instance HasTextElementWithStrategy 'Default syntax where
  isTextElementWithStrategy _ _ = False

instance CustomHasTextElement syntax => HasTextElementWithStrategy 'Custom syntax where
  isTextElementWithStrategy _ = customIsTextElement
