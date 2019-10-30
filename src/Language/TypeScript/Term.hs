{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.TypeScript.Term
( Syntax
, Term(..)
) where

import Control.Lens.Lens
import Data.Abstract.Declarations
import Data.Abstract.FreeVariables
import Data.Aeson (ToJSON)
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Graph.ControlFlowVertex (VertexDeclaration(..), toVertex1)
import qualified Data.Sum as Sum
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import Data.Traversable
import Diffing.Interpreter
import qualified Language.TypeScript.Syntax as TypeScript.Syntax
import Source.Loc
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
  , TypeScript.Syntax.TypeParameter
  , TypeScript.Syntax.Constraint
  , TypeScript.Syntax.ParenthesizedType
  , TypeScript.Syntax.DefaultType
  , TypeScript.Syntax.PredefinedType
  , TypeScript.Syntax.TypeIdentifier
  , TypeScript.Syntax.NestedIdentifier
  , TypeScript.Syntax.NestedTypeIdentifier
  , TypeScript.Syntax.GenericType
  , TypeScript.Syntax.TypeArguments
  , TypeScript.Syntax.TypePredicate
  , TypeScript.Syntax.CallSignature
  , TypeScript.Syntax.ConstructSignature
  , TypeScript.Syntax.ArrayType
  , TypeScript.Syntax.LookupType
  , TypeScript.Syntax.FlowMaybeType
  , TypeScript.Syntax.TypeQuery
  , TypeScript.Syntax.IndexTypeQuery
  , TypeScript.Syntax.ThisType
  , TypeScript.Syntax.ExistentialType
  , TypeScript.Syntax.AbstractMethodSignature
  , TypeScript.Syntax.IndexSignature
  , TypeScript.Syntax.ObjectType
  , TypeScript.Syntax.LiteralType
  , TypeScript.Syntax.Union
  , TypeScript.Syntax.Intersection
  , TypeScript.Syntax.Module
  , TypeScript.Syntax.InternalModule
  , TypeScript.Syntax.FunctionType
  , TypeScript.Syntax.Tuple
  , TypeScript.Syntax.Constructor
  , TypeScript.Syntax.TypeAssertion
  , TypeScript.Syntax.ImportAlias
  , TypeScript.Syntax.Debugger
  , TypeScript.Syntax.ShorthandPropertyIdentifier
  , TypeScript.Syntax.Super
  , TypeScript.Syntax.Undefined
  , TypeScript.Syntax.ClassHeritage
  , TypeScript.Syntax.AbstractClass
  , TypeScript.Syntax.ImplementsClause
  , TypeScript.Syntax.OptionalParameter
  , TypeScript.Syntax.RequiredParameter
  , TypeScript.Syntax.RestParameter
  , TypeScript.Syntax.PropertySignature
  , TypeScript.Syntax.AmbientDeclaration
  , TypeScript.Syntax.EnumDeclaration
  , TypeScript.Syntax.ExtendsClause
  , TypeScript.Syntax.AmbientFunction
  , TypeScript.Syntax.ImportRequireClause
  , TypeScript.Syntax.ImportClause
  , TypeScript.Syntax.LabeledStatement
  , TypeScript.Syntax.Annotation
  , TypeScript.Syntax.With
  , TypeScript.Syntax.ForOf
  , TypeScript.Syntax.Update
  , TypeScript.Syntax.ComputedPropertyName
  , TypeScript.Syntax.Decorator
  , TypeScript.Syntax.Import
  , TypeScript.Syntax.QualifiedAliasedImport
  , TypeScript.Syntax.SideEffectImport
  , TypeScript.Syntax.DefaultExport
  , TypeScript.Syntax.QualifiedExport
  , TypeScript.Syntax.QualifiedExportFrom
  , TypeScript.Syntax.JavaScriptRequire
  , []
  , Statement.StatementBlock
  , TypeScript.Syntax.MetaProperty
  , TypeScript.Syntax.AnnotatedExpression
  ]


newtype Term ann = Term { getTerm :: Term.TermF (Sum.Sum Syntax) ann (Term ann) }
  deriving (Eq, Declarations, FreeVariables, Ord, Show, ToJSON)

instance Term.IsTerm Term where
  type Syntax Term = Sum.Sum Syntax
  toTermF = coerce
  fromTermF = coerce

instance Foldable Term where
  foldMap = foldMapDefault

instance Functor Term where
  fmap = fmapDefault

instance Traversable Term where
  traverse f = go where go = fmap Term . bitraverse f go . getTerm

instance VertexDeclaration Term where
  toVertex info (Term (Term.In ann syntax)) = toVertex1 ann info syntax

instance Syntax.HasErrors Term where
  getErrors = cata $ \ (Term.In Loc{..} syntax) ->
    maybe (fold syntax) (pure . Syntax.unError span) (Sum.project syntax)


instance DiffTerms Term where
  diffTermPair = diffTermPair . bimap (cata Term.Term) (cata Term.Term)

type instance Base (Term ann) = Term.TermF (Sum.Sum Syntax) ann

instance Recursive (Term ann) where
  project = getTerm

instance HasSpan ann => HasSpan (Term ann) where
  span_ = inner.span_ where inner = lens getTerm (\t i -> t { getTerm = i })
  {-# INLINE span_ #-}
