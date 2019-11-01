{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.Go.Term
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
import Language.Go.Syntax as Go.Syntax
import Language.Go.Type as Go.Type
import Source.Loc
import Source.Span

type Syntax =
  [ Comment.Comment
  , Declaration.Constructor
  , Declaration.Function
  , Declaration.Method
  , Declaration.MethodSignature
  , Declaration.Type
  , Declaration.TypeAlias
  , Expression.Plus
  , Expression.Minus
  , Expression.Times
  , Expression.DividedBy
  , Expression.Modulo
  , Expression.Power
  , Expression.Negate
  , Expression.FloorDivision
  , Expression.BOr
  , Expression.BAnd
  , Expression.BXOr
  , Expression.LShift
  , Expression.RShift
  , Expression.UnsignedRShift
  , Expression.Complement
  , Expression.Call
  , Expression.LessThan
  , Expression.LessThanEqual
  , Expression.GreaterThan
  , Expression.GreaterThanEqual
  , Expression.Equal
  , Expression.StrictEqual
  , Expression.Comparison
  , Expression.Subscript
  , Expression.Member
  , Statement.PostDecrement
  , Statement.PostIncrement
  , Expression.MemberAccess
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Go.Syntax.Composite
  , Go.Syntax.DefaultPattern
  , Go.Syntax.Defer
  , Go.Syntax.Field
  , Go.Syntax.Go
  , Go.Syntax.Label
  , Go.Syntax.Package
  , Go.Syntax.Receive
  , Go.Syntax.ReceiveOperator
  , Go.Syntax.Rune
  , Go.Syntax.Select
  , Go.Syntax.Send
  , Go.Syntax.Slice
  , Go.Syntax.TypeAssertion
  , Go.Syntax.TypeConversion
  , Go.Syntax.TypeSwitch
  , Go.Syntax.TypeSwitchGuard
  , Go.Syntax.Variadic
  , Go.Type.BidirectionalChannel
  , Go.Type.ReceiveChannel
  , Go.Type.SendChannel
  , Go.Syntax.Import
  , Go.Syntax.QualifiedImport
  , Go.Syntax.SideEffectImport
  , Literal.Array
  , Literal.Complex
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Pointer
  , Literal.Reference
  , Literal.TextElement
  , Statement.Assignment
  , Statement.Break
  , Statement.Continue
  , Statement.For
  , Statement.ForEach
  , Statement.Goto
  , Statement.If
  , Statement.Match
  , Statement.NoOp
  , Statement.Pattern
  , Statement.Return
  , Statement.Statements
  , Syntax.Context
  , Syntax.Error
  , Syntax.Empty
  , Syntax.Identifier
  , Type.Annotation
  , Type.Array
  , Type.Function
  , Type.Interface
  , Type.Map
  , Type.Parenthesized
  , Type.Pointer
  , Type.Slice
  , []
  , Literal.String
  , Literal.EscapeSequence
  , Literal.Null
  , Literal.Boolean
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
