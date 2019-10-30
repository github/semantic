{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.PHP.Term
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
import qualified Language.PHP.Syntax as Syntax
import Source.Loc
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
