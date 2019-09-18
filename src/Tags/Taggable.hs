{- |

Taggable allows projecting syntax terms to a list of named symbols. In order to
identify a new syntax as Taggable, you need to:

1. Give that syntax a non-derived Taggable instance and implement at least the
'symbolName' method.

2. Make sure that 'symbolsToSummarize' in Tagging.hs includes the string
constructor name of this syntax.

-}
{-# LANGUAGE AllowAmbiguousTypes, GADTs, ConstraintKinds, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Tags.Taggable
( Tagger
, Token(..)
, Taggable(..)
, IsTaggable
, HasTextElement
, subtractLocation
, tagging
)
where

import Prologue

import Analysis.ConstructorName
import Analysis.HasTextElement
import Data.Abstract.Declarations
import Data.Abstract.Name
import Data.Blob
import Data.Language
import Data.Location
import Data.Range
import Data.Term
import Data.Text hiding (empty)

import Streaming hiding (Sum)
import Streaming.Prelude (yield)

import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Language.Ruby.Syntax as Ruby


 -- TODO: Move to src/Data
data Token
  = Enter { tokenName :: Text, tokenSnippetRange :: Maybe Range }
  | Exit  { tokenName :: Text, tokenSnippetRange :: Maybe Range}
  | Iden  { identifierName :: Text, tokenSpan :: Span, docsLiteralRange :: Maybe Range }
  deriving (Eq, Show)

type Tagger = Stream (Of Token)

enter, exit :: Monad m => String -> Maybe Range -> Tagger m ()
enter c = yield . Enter (pack c)
exit c = yield . Exit (pack c)

emitIden :: Monad m => Span -> Maybe Range -> Name -> Tagger m ()
emitIden span docsLiteralRange name = yield (Iden (formatName name) span docsLiteralRange)

class (Show1 constr, Traversable constr) => Taggable constr where
  docsLiteral ::
    ( Functor syntax
    , Foldable syntax
    , HasTextElement syntax
    )
    => Language -> constr (Term syntax Location) -> Maybe Range
  docsLiteral _ _ = Nothing

  snippet :: (Foldable syntax) => Location -> constr (Term syntax Location) -> Maybe Range
  snippet _ _ = Nothing

  symbolName :: Declarations1 syntax => constr (Term syntax Location) -> Maybe Name
  symbolName _ = Nothing

type IsTaggable syntax =
  ( Functor syntax
  , Foldable syntax
  , Traversable syntax
  , Show1 syntax
  , Taggable syntax
  , ConstructorName syntax
  , Declarations1 syntax
  , HasTextElement syntax
  )

tagging :: (Monad m, IsTaggable syntax)
        => Blob
        -> Term syntax Location
        -> Stream (Of Token) m ()
tagging b = foldSubterms (descend (blobLanguage b))

descend ::
  ( Taggable (TermF syntax Location)
  , ConstructorName (TermF syntax Location)
  , Functor syntax
  , Foldable syntax
  , HasTextElement syntax
  , Declarations1 syntax
  , Monad m
  )
  => Language -> SubtermAlgebra (TermF syntax Location) (Term syntax Location) (Tagger m ())
descend lang t@(In loc _) = do
  let term = fmap subterm t
  let snippetRange = snippet loc term
  let litRange = docsLiteral lang term

  enter (constructorName term) snippetRange
  maybe (pure ()) (emitIden (locationSpan loc) litRange) (symbolName term)
  traverse_ subtermRef t
  exit (constructorName term) snippetRange

subtractLocation :: Location -> Location -> Range
subtractLocation a b = subtractRange (locationByteRange a) (locationByteRange b)

-- Instances

instance ( Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Taggable fs) => Taggable (Sum fs) where
  docsLiteral a = apply @Taggable (docsLiteral a)
  snippet x = apply @Taggable (snippet x)
  symbolName = apply @Taggable symbolName

instance (Taggable a) => Taggable (TermF a Location) where
  docsLiteral l t = docsLiteral l (termFOut t)
  snippet ann t = snippet ann (termFOut t)
  symbolName t = symbolName (termFOut t)

instance Taggable Declaration.Function where
  docsLiteral Python (Declaration.Function _ _ _ (Term (In _ bodyF)))
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral _ _         = Nothing
  snippet ann (Declaration.Function _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName = declaredName . Declaration.functionName

instance Taggable Declaration.Method where
  docsLiteral Python (Declaration.Method _ _ _ _ (Term (In _ bodyF)) _)
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral _ _         = Nothing
  snippet ann (Declaration.Method _ _ _ _ (Term (In body _)) _) = Just $ subtractLocation ann body
  symbolName = declaredName . Declaration.methodName

instance Taggable Declaration.Class where
  docsLiteral Python (Declaration.Class _ _ _ (Term (In _ bodyF)))
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral _ _         = Nothing
  snippet ann (Declaration.Class _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName = declaredName . Declaration.classIdentifier

instance Taggable Ruby.Class where
  snippet ann (Ruby.Class _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName = declaredName . Ruby.classIdentifier

instance Taggable Ruby.Module where
  snippet ann (Ruby.Module _ (Term (In body _):_)) = Just $ subtractLocation ann body
  snippet ann (Ruby.Module _ _)                    = Just $ locationByteRange ann
  symbolName = declaredName . Ruby.moduleIdentifier

instance Taggable Expression.Call where
  snippet ann (Expression.Call _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName = declaredName . Expression.callFunction

instance Taggable Ruby.Send where
  snippet ann (Ruby.Send _ _ _ (Just (Term (In body _)))) = Just $ subtractLocation ann body
  snippet ann _                                           = Just $ locationByteRange ann
  symbolName Ruby.Send{..} = declaredName =<< sendSelector

instance Taggable []

instance Taggable Expression.And
instance Taggable Expression.Await
instance Taggable Expression.BAnd
instance Taggable Expression.BOr
instance Taggable Expression.BXOr
instance Taggable Expression.Cast
instance Taggable Expression.Comparison
instance Taggable Expression.Complement
instance Taggable Expression.Delete
instance Taggable Expression.DividedBy
instance Taggable Expression.Enumeration
instance Taggable Expression.Equal
instance Taggable Expression.FloorDivision
instance Taggable Expression.GreaterThan
instance Taggable Expression.GreaterThanEqual
instance Taggable Expression.InstanceOf
instance Taggable Expression.LessThan
instance Taggable Expression.LessThanEqual
instance Taggable Expression.LShift
instance Taggable Expression.Matches
instance Taggable Expression.Member
instance Taggable Expression.MemberAccess
instance Taggable Expression.Minus
instance Taggable Expression.Modulo
instance Taggable Expression.Negate
instance Taggable Expression.New
instance Taggable Expression.NonNullExpression
instance Taggable Expression.Not
instance Taggable Expression.NotMatches
instance Taggable Expression.Or
instance Taggable Expression.Plus
instance Taggable Expression.Power
instance Taggable Expression.RShift
instance Taggable Expression.ScopeResolution
instance Taggable Expression.SequenceExpression
instance Taggable Expression.StrictEqual
instance Taggable Expression.Subscript
instance Taggable Expression.Super
instance Taggable Expression.This
instance Taggable Expression.Times
instance Taggable Expression.Typeof
instance Taggable Expression.UnsignedRShift
instance Taggable Expression.Void
instance Taggable Expression.XOr

instance Taggable Literal.Boolean
instance Taggable Literal.Integer
instance Taggable Literal.Float
instance Taggable Literal.Rational
instance Taggable Literal.Complex
instance Taggable Literal.String
instance Taggable Literal.Character
instance Taggable Literal.InterpolationElement
instance Taggable Literal.TextElement
instance Taggable Literal.EscapeSequence
instance Taggable Literal.Symbol
instance Taggable Literal.SymbolElement
instance Taggable Literal.Regex
instance Taggable Literal.Array
instance Taggable Literal.Hash
instance Taggable Literal.Tuple
instance Taggable Literal.Set
instance Taggable Literal.Pointer
instance Taggable Literal.Reference
instance Taggable Literal.Null
instance Taggable Literal.KeyValue

instance Taggable Statement.Assignment
instance Taggable Statement.Break
instance Taggable Statement.Catch
instance Taggable Statement.Continue
instance Taggable Statement.DoWhile
instance Taggable Statement.Else
instance Taggable Statement.Finally
instance Taggable Statement.For
instance Taggable Statement.ForEach
instance Taggable Statement.Goto
instance Taggable Statement.If
instance Taggable Statement.Let
instance Taggable Statement.Match
instance Taggable Statement.NoOp
instance Taggable Statement.Pattern
instance Taggable Statement.PostDecrement
instance Taggable Statement.PostIncrement
instance Taggable Statement.PreDecrement
instance Taggable Statement.PreIncrement
instance Taggable Statement.Retry
instance Taggable Statement.Return
instance Taggable Statement.ScopeEntry
instance Taggable Statement.ScopeExit
instance Taggable Statement.StatementBlock
instance Taggable Statement.Statements
instance Taggable Statement.Throw
instance Taggable Statement.Try
instance Taggable Statement.While
instance Taggable Statement.Yield

instance Taggable Declaration.Comprehension
instance Taggable Declaration.Constructor
instance Taggable Declaration.Datatype
instance Taggable Declaration.Decorator
instance Taggable Declaration.InterfaceDeclaration
instance Taggable Declaration.MethodSignature
instance Taggable Declaration.OptionalParameter
instance Taggable Declaration.PublicFieldDefinition
instance Taggable Declaration.RequiredParameter
instance Taggable Declaration.Type
instance Taggable Declaration.TypeAlias
instance Taggable Declaration.Variable
instance Taggable Declaration.VariableDeclaration

instance Taggable Ruby.Require
instance Taggable Ruby.Load
instance Taggable Ruby.LowPrecedenceAnd
instance Taggable Ruby.LowPrecedenceOr
instance Taggable Ruby.Assignment
instance Taggable Ruby.ZSuper
