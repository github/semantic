{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.TSX.Term
( Syntax
, Term(..)
, Diff(..)
) where

import Control.Lens.Lens
import Data.Abstract.Declarations
import Data.Abstract.FreeVariables
import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Foldable
import Data.Graph.ControlFlowVertex (VertexDeclaration)
import Data.Sum (Sum)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Diff as Diff
import qualified Data.Term as Term
import Diffing.Interpreter
import qualified Language.TSX.Syntax as TSX.Syntax
import Source.Span

type Syntax =
  [ Comment.Comment
  , Comment.HashBang
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.MethodSignature
  , Declaration.InterfaceDeclaration
  , Declaration.PublicFieldDefinition
  , Declaration.VariableDeclaration
  , Declaration.TypeAlias
  , Expression.Plus
  , Expression.Minus
  , Expression.Times
  , Expression.DividedBy
  , Expression.Modulo
  , Expression.Power
  , Expression.Negate
  , Expression.FloorDivision
  , Expression.BAnd
  , Expression.BOr
  , Expression.BXOr
  , Expression.LShift
  , Expression.RShift
  , Expression.UnsignedRShift
  , Expression.Complement
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Expression.Call
  , Expression.Cast
  , Expression.LessThan
  , Expression.LessThanEqual
  , Expression.GreaterThan
  , Expression.GreaterThanEqual
  , Expression.Equal
  , Expression.StrictEqual
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.MemberAccess
  , Expression.NonNullExpression
  , Expression.ScopeResolution
  , Expression.SequenceExpression
  , Expression.Subscript
  , Expression.Member
  , Expression.Delete
  , Expression.Void
  , Expression.Typeof
  , Expression.InstanceOf
  , Expression.New
  , Expression.Await
  , Expression.This
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
  , Statement.DoWhile
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
  , Statement.Statements
  , Statement.Throw
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.AccessibilityModifier
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Context
  , Type.Readonly
  , Type.TypeParameters
  , TSX.Syntax.TypeParameter
  , TSX.Syntax.Constraint
  , TSX.Syntax.ParenthesizedType
  , TSX.Syntax.DefaultType
  , TSX.Syntax.PredefinedType
  , TSX.Syntax.TypeIdentifier
  , TSX.Syntax.NestedIdentifier
  , TSX.Syntax.NestedTypeIdentifier
  , TSX.Syntax.GenericType
  , TSX.Syntax.TypeArguments
  , TSX.Syntax.TypePredicate
  , TSX.Syntax.CallSignature
  , TSX.Syntax.ConstructSignature
  , TSX.Syntax.ArrayType
  , TSX.Syntax.LookupType
  , TSX.Syntax.FlowMaybeType
  , TSX.Syntax.TypeQuery
  , TSX.Syntax.IndexTypeQuery
  , TSX.Syntax.ThisType
  , TSX.Syntax.ExistentialType
  , TSX.Syntax.AbstractMethodSignature
  , TSX.Syntax.IndexSignature
  , TSX.Syntax.ObjectType
  , TSX.Syntax.LiteralType
  , TSX.Syntax.Union
  , TSX.Syntax.Intersection
  , TSX.Syntax.Module
  , TSX.Syntax.InternalModule
  , TSX.Syntax.FunctionType
  , TSX.Syntax.Tuple
  , TSX.Syntax.Constructor
  , TSX.Syntax.TypeAssertion
  , TSX.Syntax.ImportAlias
  , TSX.Syntax.Debugger
  , TSX.Syntax.ShorthandPropertyIdentifier
  , TSX.Syntax.Super
  , TSX.Syntax.Undefined
  , TSX.Syntax.ClassHeritage
  , TSX.Syntax.AbstractClass
  , TSX.Syntax.ImplementsClause
  , TSX.Syntax.JsxElement
  , TSX.Syntax.JsxSelfClosingElement
  , TSX.Syntax.JsxOpeningElement
  , TSX.Syntax.JsxText
  , TSX.Syntax.JsxClosingElement
  , TSX.Syntax.JsxExpression
  , TSX.Syntax.JsxAttribute
  , TSX.Syntax.JsxFragment
  , TSX.Syntax.JsxNamespaceName
  , TSX.Syntax.OptionalParameter
  , TSX.Syntax.RequiredParameter
  , TSX.Syntax.RestParameter
  , TSX.Syntax.PropertySignature
  , TSX.Syntax.AmbientDeclaration
  , TSX.Syntax.EnumDeclaration
  , TSX.Syntax.ExtendsClause
  , TSX.Syntax.AmbientFunction
  , TSX.Syntax.ImportRequireClause
  , TSX.Syntax.ImportClause
  , TSX.Syntax.LabeledStatement
  , TSX.Syntax.Annotation
  , TSX.Syntax.With
  , TSX.Syntax.ForOf
  , TSX.Syntax.Update
  , TSX.Syntax.ComputedPropertyName
  , TSX.Syntax.Decorator
  , TSX.Syntax.Import
  , TSX.Syntax.QualifiedAliasedImport
  , TSX.Syntax.SideEffectImport
  , TSX.Syntax.DefaultExport
  , TSX.Syntax.QualifiedExport
  , TSX.Syntax.QualifiedExportFrom
  , TSX.Syntax.JavaScriptRequire
  , []
  , Statement.StatementBlock
  , TSX.Syntax.MetaProperty
  , TSX.Syntax.AnnotatedExpression
  ]


newtype Term ann = Term { getTerm :: Term.Term (Sum Syntax) ann }
  deriving (Eq, Declarations, Foldable, FreeVariables, Functor, Syntax.HasErrors, Ord, Show, Traversable, VertexDeclaration)

newtype Diff ann1 ann2 = Diff { getDiff :: Diff.Diff (Sum Syntax) ann1 ann2 }
  deriving (Bifoldable, Bifunctor)

instance DiffTerms Term where
  type DiffFor Term = Diff
  diffTermPair = Diff . diffTermPair . bimap getTerm getTerm

type instance Base (Term ann) = Term.TermF (Sum Syntax) ann

instance Recursive (Term ann) where
  project = fmap Term . project . getTerm

instance HasSpan ann => HasSpan (Term ann) where
  span_ = inner.span_ where inner = lens getTerm (\t i -> t { getTerm = i })
  {-# INLINE span_ #-}
