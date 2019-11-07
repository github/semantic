{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.Ruby.Term
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
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Term as Term
import Data.Traversable
import Diffing.Interpreter
import qualified Language.Ruby.Syntax as Ruby.Syntax
import Source.Loc
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
