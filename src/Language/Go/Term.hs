{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.Go.Term
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
import Language.Go.Syntax as Go.Syntax
import Language.Go.Type as Go.Type
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
