{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.Python.Term
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
import Language.Python.Syntax as Python.Syntax
import Source.Loc
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
