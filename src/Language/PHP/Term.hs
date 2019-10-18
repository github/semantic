{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.PHP.Term
( Syntax
, Term(..)
, Diff(..)
) where

import Control.Lens.Lens
import Data.Abstract.Declarations
import Data.Abstract.FreeVariables
import Data.Bifoldable
import Data.Bifunctor
import qualified Data.Diff as Diff
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
import qualified Data.Term as Term
import Diffing.Interpreter
import qualified Language.PHP.Syntax as Syntax
import Source.Span

type Syntax =
  [ Comment.Comment
  , Declaration.Class
  , Declaration.Function
  , Declaration.Method
  , Declaration.VariableDeclaration
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
  , Expression.InstanceOf
  , Expression.MemberAccess
  , Expression.New
  , Expression.SequenceExpression
  , Expression.Subscript
  , Expression.Member
  , Literal.Array
  , Literal.Float
  , Literal.Integer
  , Literal.KeyValue
  , Literal.TextElement
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.DoWhile
  , Statement.Else
  , Statement.Finally
  , Statement.For
  , Statement.ForEach
  , Statement.Goto
  , Statement.If
  , Statement.Match
  , Statement.Pattern
  , Statement.Return
  , Statement.Statements
  , Statement.Throw
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.AliasAs
  , Syntax.ArrayElement
  , Syntax.BaseTypeDeclaration
  , Syntax.CastType
  , Syntax.ClassBaseClause
  , Syntax.ClassConstDeclaration
  , Syntax.ClassInterfaceClause
  , Syntax.ClassModifier
  , Syntax.Clone
  , Syntax.ConstDeclaration
  , Syntax.ConstructorDeclaration
  , Syntax.Context
  , Syntax.Declare
  , Syntax.DeclareDirective
  , Syntax.DestructorDeclaration
  , Syntax.Echo
  , Syntax.Empty
  , Syntax.EmptyIntrinsic
  , Syntax.Error
  , Syntax.ErrorControl
  , Syntax.EvalIntrinsic
  , Syntax.ExitIntrinsic
  , Syntax.GlobalDeclaration
  , Syntax.Identifier
  , Syntax.Include
  , Syntax.IncludeOnce
  , Syntax.InsteadOf
  , Syntax.InterfaceBaseClause
  , Syntax.InterfaceDeclaration
  , Syntax.IssetIntrinsic
  , Syntax.LabeledStatement
  , Syntax.Namespace
  , Syntax.NamespaceAliasingClause
  , Syntax.NamespaceName
  , Syntax.NamespaceUseClause
  , Syntax.NamespaceUseDeclaration
  , Syntax.NamespaceUseGroupClause
  , Syntax.NewVariable
  , Syntax.PrintIntrinsic
  , Syntax.PropertyDeclaration
  , Syntax.PropertyModifier
  , Syntax.QualifiedName
  , Syntax.RelativeScope
  , Syntax.Require
  , Syntax.RequireOnce
  , Syntax.ReturnType
  , Syntax.ScalarType
  , Syntax.ShellCommand
  , Syntax.Concat
  , Syntax.SimpleVariable
  , Syntax.Static
  , Syntax.Text
  , Syntax.TraitDeclaration
  , Syntax.TraitUseClause
  , Syntax.TraitUseSpecification
  , Syntax.TypeDeclaration
  , Syntax.Unset
  , Syntax.Update
  , Syntax.UseClause
  , Syntax.VariableName
  , Type.Annotation
  , []
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
