{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.Python.Term
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
import Language.Python.Syntax as Python.Syntax
import Source.Span

type Syntax =
  [ Comment.Comment
  , Declaration.Class
  , Declaration.Comprehension
  , Declaration.Decorator
  , Declaration.Function
  , Declaration.RequiredParameter
  , Expression.Plus
  , Expression.Minus
  , Expression.Times
  , Expression.DividedBy
  , Expression.Modulo
  , Expression.Power
  , Expression.Negate
  , Expression.FloorDivision
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Expression.BAnd
  , Expression.BOr
  , Expression.BXOr
  , Expression.LShift
  , Expression.RShift
  , Expression.Complement
  , Expression.Call
  , Expression.LessThan
  , Expression.LessThanEqual
  , Expression.GreaterThan
  , Expression.GreaterThanEqual
  , Expression.Equal
  , Expression.StrictEqual
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.ScopeResolution
  , Expression.MemberAccess
  , Expression.Subscript
  , Expression.Member
  , Literal.Array
  , Literal.Boolean
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.Set
  , Literal.String
  , Literal.TextElement
  , Literal.Tuple
  , Python.Syntax.Alias
  , Python.Syntax.Ellipsis
  , Python.Syntax.FutureImport
  , Python.Syntax.Import
  , Python.Syntax.QualifiedImport
  , Python.Syntax.QualifiedAliasedImport
  , Python.Syntax.Redirect
  , Statement.Assignment
  , Statement.Break
  , Statement.Catch
  , Statement.Continue
  , Statement.Else
  , Statement.Finally
  , Statement.ForEach
  , Statement.If
  , Statement.Let
  , Statement.NoOp
  , Statement.Return
  , Statement.Statements
  , Statement.Throw
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
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
