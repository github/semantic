{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.Ruby.Term
( Syntax
, Term(..)
) where

import Control.Lens.Lens
import Data.Abstract.Declarations
import Data.Abstract.FreeVariables
import Data.Bifunctor
import Data.Diff
import Data.Functor.Foldable
import Data.Graph.ControlFlowVertex (VertexDeclaration)
import Data.Sum (Sum)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Term as Term
import Diffing.Interpreter
import qualified Language.Ruby.Syntax as Ruby.Syntax
import Source.Span

type Syntax =
  [ Comment.Comment
  , Declaration.Function
  , Declaration.Method
  , Directive.File
  , Directive.Line
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
  , Expression.Complement
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Expression.Call
  , Expression.LessThan
  , Expression.LessThanEqual
  , Expression.GreaterThan
  , Expression.GreaterThanEqual
  , Expression.Equal
  , Expression.StrictEqual
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.Matches
  , Expression.NotMatches
  , Expression.MemberAccess
  , Expression.ScopeResolution
  , Expression.Subscript
  , Expression.Member
  , Expression.This
  , Literal.Array
  , Literal.Boolean
  , Literal.Character
  , Literal.Complex
  , Literal.EscapeSequence
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.InterpolationElement
  , Literal.KeyValue
  , Literal.Null
  , Literal.Rational
  , Literal.Regex
  , Literal.String
  , Literal.Symbol
  , Literal.SymbolElement
  , Literal.TextElement
  , Ruby.Syntax.Assignment
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
  , Statement.Statements
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Ruby.Syntax.Class
  , Ruby.Syntax.Load
  , Ruby.Syntax.LowPrecedenceAnd
  , Ruby.Syntax.LowPrecedenceOr
  , Ruby.Syntax.Module
  , Ruby.Syntax.Require
  , Ruby.Syntax.Send
  , Ruby.Syntax.ZSuper
  , []
  ]


newtype Term ann = Term { getTerm :: Term.Term (Sum Syntax) ann }
  deriving (Eq, Declarations, Foldable, FreeVariables, Functor, Syntax.HasErrors, Ord, Show, Traversable, VertexDeclaration)

instance DiffTerms Term where
  type DiffFor Term = Diff (Sum Syntax)
  diffTermPair = diffTermPair . bimap getTerm getTerm

type instance Base (Term ann) = Term.TermF (Sum Syntax) ann

instance Recursive (Term ann) where
  project = fmap Term . project . getTerm

instance HasSpan ann => HasSpan (Term ann) where
  span_ = inner.span_ where inner = lens getTerm (\t i -> t { getTerm = i })
  {-# INLINE span_ #-}
