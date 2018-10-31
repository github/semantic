{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Taggable
( Tagger
, Token(..)
, Taggable(..)
, tagging
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Analysis.ConstructorName
import           Control.Matching hiding (target)
import           Control.Arrow hiding (first)
import           Control.Effect as Eff
import           Control.Effect.Error as Error
import qualified Control.Effect.State as State
import           Control.Monad
import           Control.Monad.Trans
import           Control.Rewriting hiding (apply)
import           Data.Abstract.Declarations
import           Data.Abstract.Name
import           Data.Blob
import           Data.History
import           Data.List (intersperse)
import           Data.Location
import           Data.Machine as Machine
import           Data.Range
import qualified Data.Source as Source
import           Data.Span
import           Data.Term
import           Data.Text hiding (empty)

import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Type as Type
import qualified Data.Syntax as Syntax
import qualified Language.Python.Syntax as Python
import qualified Language.Ruby.Syntax as Ruby

 -- TODO: Move to src/Data
data Token
  = Enter Text (Maybe Range)
  | Exit Text (Maybe Range)
  | Identifier Text (Maybe Range)
  deriving (Eq, Show)

data Tagger a where
  Pure :: a -> Tagger a
  Bind :: Tagger a -> (a -> Tagger b) -> Tagger b
  Tell :: Token -> Tagger ()
  Get :: Tagger InternalState
  Put :: InternalState -> Tagger ()

compile :: InternalState -> Tagger a -> Machine.Plan k Token (InternalState, a)
compile p = \case
  Pure a   -> pure (p, a)
  Bind a f -> compile p a >>= (\(new, v) -> compile new (f v))
  Tell t   -> Machine.yield t $> (p, ())
  Get      -> pure (p, p)
  Put p'   -> pure (p', ())

instance Functor Tagger where fmap = liftA
instance Applicative Tagger where
  pure  = Pure
  (<*>) = ap
instance Monad Tagger where (>>=) = Bind

enter, exit :: String -> Maybe Range -> Tagger ()
enter c = Tell . Enter (pack c)
exit c = Tell . Exit (pack c)

emitIden :: Maybe Range -> Name -> Tagger ()
emitIden range name = Tell (Identifier (formatName name) range)

newtype InternalState = InternalState { location  :: Location}

 -- InternalState handling
asks :: (InternalState -> a) -> Tagger a
asks f = f <$> Get

modify :: (InternalState -> InternalState) -> Tagger ()
modify f = Get >>= \x -> Put . f $! x

class (Show1 syntax, Traversable syntax, ConstructorName syntax) => Taggable syntax where
  tag :: Maybe Range -> Maybe Name -> Maybe Range -> FAlgebra syntax (Tagger ())
  tag r name range t = do
    let cName = constructorName t
    enter cName range
    maybe (pure ()) (emitIden r) name
    sequenceA_ t
    exit cName range

  docsLiteral ::
    ( [] :< fs
    , Declaration.Function :< fs
    , Literal.TextElement :< fs
    , Apply Functor fs
    , Apply Foldable fs
    )
    => Location -> syntax (Term (Sum fs) Location) -> Maybe Range
  docsLiteral _ _ = Nothing

  snippet :: Location -> syntax (Term a Location) -> Maybe Range
  snippet _ _ = Nothing

withLocation :: Annotated t Location => t -> Tagger a -> Tagger a
withLocation t act = do
  old <- asks location
  modify (\x -> x { location = annotation t })
  act <* modify (\x -> x { location = old })

tagging ::
  ( Apply Functor fs
  , Apply Foldable fs
  , Apply Traversable fs
  , Apply Show1 fs
  , Apply Taggable fs
  , Apply ConstructorName fs
  , Apply Declarations1 fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => Blob
  -> Term (Sum fs) Location
  -> Machine.Source Token
tagging Blob{..} term = pipe
  where pipe  = Machine.construct . fmap snd $ compile state go
        state = InternalState (termAnnotation term)
        go    = foldSubterms descend term

descend :: forall syntax fs.
  ( Taggable syntax
  , Declarations (syntax (Term (Sum fs) Location))
  , Functor syntax
  , Apply Functor fs
  , Apply Foldable fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => SubtermAlgebra syntax (Term (Sum fs) Location) (Tagger ())
descend t = do
  (InternalState loc) <- asks id
  let n = declaredName (fmap subterm t)
  let range = docsLiteral loc (fmap subterm t)
  let r = snippet loc (fmap subterm t)
  tag range n r (fmap subtermRef t)

getRangeUntil :: Location -> Rule () (syntax (Term a Location)) (Term a Location) -> syntax (Term a Location) -> Maybe Range
getRangeUntil a finder r
  = let start = locationByteRange a
        end   = locationByteRange <$> rewrite (finder >>^ annotation) () r
    in either (const Nothing) (Just . subtractRange start) end

getRange :: (Corecursive t, Recursive t, Foldable (Base t)) => Matcher t Location -> t -> Maybe Range
getRange matcher t = locationByteRange <$> runMatcher @Maybe matcher t

firstItem :: Rule env [a] a
firstItem = target >>= maybeM (Prologue.fail "empty list") . listToMaybe

docstringMatcher ::
  ( [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  , term ~ Term (Sum fs) Location
  )
  => Matcher term Location
docstringMatcher = match Declaration.functionBody $ do
  (a:_) <- narrow
  (Literal.TextElement _) <- guardTerm a
  pure (annotation a)

-- Instances

instance ( Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Taggable fs, Apply ConstructorName fs) => Taggable (Sum fs) where
  tag d n r = apply @Taggable (tag d n r)
  docsLiteral a = apply @Taggable (docsLiteral a)
  snippet x = apply @Taggable (snippet x)

instance (Taggable a) => Taggable (TermF a Location) where
  tag d n r t = withLocation t (tag d n r (termFOut t))
  docsLiteral _ t = docsLiteral (termFAnnotation t) (termFOut t)
  snippet _ t = snippet (termFAnnotation t) (termFOut t)

instance Taggable [] where
  tag _ _ _ = sequenceA_

instance Taggable Declaration.Function where
  docsLiteral a = getRange docstringMatcher . injectTerm a
  snippet ann = getRangeUntil ann (arr Declaration.functionBody)

instance Taggable Declaration.Method

instance Taggable Syntax.Context where
  snippet ann = getRangeUntil ann (arr Syntax.contextSubject)

instance Taggable Comment.Comment

instance Taggable Expression.And
instance Taggable Expression.Await
instance Taggable Expression.BAnd
instance Taggable Expression.BOr
instance Taggable Expression.BXOr
instance Taggable Expression.Call
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
instance Taggable Statement.Statements
instance Taggable Statement.Throw
instance Taggable Statement.Try
instance Taggable Statement.While
instance Taggable Statement.Yield

instance Taggable Syntax.Empty
instance Taggable Syntax.Error
instance Taggable Syntax.Identifier

instance Taggable Type.Annotation

instance Taggable Declaration.MethodSignature
instance Taggable Declaration.InterfaceDeclaration
instance Taggable Declaration.PublicFieldDefinition
instance Taggable Declaration.Variable
instance Taggable Declaration.Class
instance Taggable Declaration.Decorator
instance Taggable Declaration.Datatype
instance Taggable Declaration.Constructor
instance Taggable Declaration.Comprehension
instance Taggable Declaration.Type
instance Taggable Declaration.TypeAlias


instance Taggable Python.Ellipsis
instance Taggable Python.FutureImport
instance Taggable Python.Import
instance Taggable Python.QualifiedAliasedImport
instance Taggable Python.QualifiedImport
instance Taggable Python.Redirect
