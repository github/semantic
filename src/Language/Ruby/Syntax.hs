{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeOperators #-}
module Language.Ruby.Syntax
( assignment
, Syntax
, Grammar
, FAlgebra
, RAlgebra
, fToR
, identifierAlg
, cyclomaticComplexityAlg
, decoratorWithAlgebra
) where

import Data.Functor.Foldable (Base)
import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import GHC.Stack
import Language.Ruby.Grammar as Grammar
import Prologue hiding (for, get, Location, state, unless)
import Term

-- | The type of Ruby syntax.
type Syntax = Union Syntax'
type Syntax' =
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Integer
  , Literal.Range
  , Literal.String
  , Literal.Symbol
  , Statement.Assignment
  , Statement.Break
  , Statement.Continue
  , Statement.ForEach
  , Statement.If
  -- TODO: redo
  -- TODO: retry
  , Statement.Return
  , Statement.While
  , Statement.Yield
  , Syntax.Empty
  , Syntax.Identifier
  , []
  ]


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: HasCallStack => Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Program *> children (many declaration)

declaration :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
declaration = comment <|> class' <|> method

class' :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> (constant <|> scopeResolution) <*> (superclass <|> pure []) <*> many declaration)
  where superclass = pure <$ symbol Superclass <*> children constant
        scopeResolution = symbol ScopeResolution *> children (constant <|> identifier)

constant :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
constant = makeTerm <$> symbol Constant <*> (Syntax.Identifier <$> source)

identifier :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

method :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
method = makeTerm <$> symbol Method <*> children (Declaration.Method <$> identifier <*> pure [] <*> statements)

statements :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
statements = makeTerm <$> location <*> many statement

statement :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
statement  =  exit Statement.Return Return
          <|> exit Statement.Yield Yield
          <|> exit Statement.Break Break
          <|> exit Statement.Continue Next
          <|> if'
          <|> unless
          <|> while
          <|> until
          <|> for
          <|> literal
          <|> assignment'
  where exit construct sym = makeTerm <$> symbol sym <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children statement))

lvalue :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
lvalue = identifier

expression :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
expression = identifier <|> statement

comment :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

if' :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
if' =  ifElsif If
   <|> makeTerm <$> symbol IfModifier     <*> children (flip Statement.If <$> statement <*> statement <*> (makeTerm <$> location <*> pure Syntax.Empty))
  where ifElsif s = makeTerm <$> symbol s <*> children      (Statement.If <$> statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> makeTerm <$> symbol Else <*> children (many statement))))

unless :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
unless =  makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (makeTerm <$> symbol Else <*> children (many statement))))
      <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> statement <*> invert statement <*> (makeTerm <$> location <*> pure Syntax.Empty))

while :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
while =  makeTerm <$> symbol While         <*> children      (Statement.While <$> statement <*> statements)
     <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> statement <*> statement)

until :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
until =  makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert statement <*> statements)
     <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> statement <*> invert statement)

for :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> identifier <*> statement <*> statements)

assignment' :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
assignment'
   =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> lvalue <*> expression)
  <|> makeTerm <$> symbol OperatorAssignment <*> children (lvalue >>= \ var -> Statement.Assignment var <$>
         (makeTerm <$> symbol AnonPlusEqual               <*> (Expression.Plus var      <$> expression)
      <|> makeTerm <$> symbol AnonMinusEqual              <*> (Expression.Minus var     <$> expression)
      <|> makeTerm <$> symbol AnonStarEqual               <*> (Expression.Times var     <$> expression)
      <|> makeTerm <$> symbol AnonStarStarEqual           <*> (Expression.Power var     <$> expression)
      <|> makeTerm <$> symbol AnonSlashEqual              <*> (Expression.DividedBy var <$> expression)
      <|> makeTerm <$> symbol AnonPipePipeEqual           <*> (Expression.And var       <$> expression)
      <|> makeTerm <$> symbol AnonPipeEqual               <*> (Expression.BOr var       <$> expression)
      <|> makeTerm <$> symbol AnonAmpersandAmpersandEqual <*> (Expression.And var       <$> expression)
      <|> makeTerm <$> symbol AnonAmpersandEqual          <*> (Expression.BAnd var      <$> expression)
      <|> makeTerm <$> symbol AnonPercentEqual            <*> (Expression.Modulo var    <$> expression)
      <|> makeTerm <$> symbol AnonRAngleRAngleEqual       <*> (Expression.RShift var    <$> expression)
      <|> makeTerm <$> symbol AnonLAngleLAngleEqual       <*> (Expression.LShift var    <$> expression)
      <|> makeTerm <$> symbol AnonCaretEqual              <*> (Expression.BXOr var      <$> expression)))

literal :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
literal  =  makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)
        <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)
        <|> makeTerm <$> symbol Grammar.Integer <*> (Literal.Integer <$> source)
        <|> makeTerm <$> symbol Symbol <*> (Literal.Symbol <$> source)
        <|> makeTerm <$> symbol Range <*> children (Literal.Range <$> statement <*> statement) -- FIXME: represent the difference between .. and ...

invert :: (InUnion fs Expression.Boolean, HasCallStack) => Assignment (Node grammar) (Term (Union fs) Location) -> Assignment (Node grammar) (Term (Union fs) Location)
invert term = makeTerm <$> location <*> fmap Expression.Not term

makeTerm :: (InUnion fs f, HasCallStack) => a -> f (Term (Union fs) a) -> (Term (Union fs) a)
makeTerm a f = cofree $ a :< inj f

emptyTerm :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty


-- | An F-algebra on some carrier functor 'f'.
type FAlgebra f a = f a -> a

-- | An R-algebra on some carrier functor 'f' of its fixpoint type 't'.
type RAlgebra f t a = f (t, a) -> a

-- | Promote an FAlgebra into an RAlgebra (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra (Base t) a -> RAlgebra (Base t) t a
fToR f = f . fmap snd

newtype Identifier' = Identifier' ByteString
  deriving (Eq, Show)

-- | Produce the identifier for a given term, if any.
--
--   Identifier syntax is labelled, as well as declaration syntax identified by these, but other uses of these identifiers are not, e.g. the declaration of a class or method or binding of a variable will be labelled, but a function call will not.
identifierAlg :: (InUnion fs Syntax.Identifier, InUnion fs Declaration.Method, InUnion fs Declaration.Class, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) (Maybe Identifier')
identifierAlg (_ :< union) = case union of
  _ | Just (Syntax.Identifier s) <- prj union -> Just (Identifier' s)
  _ | Just Declaration.Class{..} <- prj union -> classIdentifier
  _ | Just Declaration.Method{..} <- prj union -> methodName
  _ -> Nothing

-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlg :: (InUnion fs Declaration.Method, InUnion fs Statement.Return, InUnion fs Statement.Yield, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) CyclomaticComplexity
cyclomaticComplexityAlg (_ :< union) = case union of
  _ | Just Declaration.Method{} <- prj union -> succ (sum union)
  _ | Just Statement.Return{} <- prj union -> succ (sum union)
  _ | Just Statement.Yield{} <- prj union -> succ (sum union)
  _ -> sum union

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor f
                     => RAlgebra (Base (Term f (Record fs))) (Term f (Record fs)) a -- ^ An F-algebra on terms.
                     -> Term f (Record fs) -- ^ A term to decorate with values produced by the F-algebra.
                     -> Term f (Record (a ': fs)) -- ^ A term decorated with values produced by the F-algebra.
decoratorWithAlgebra alg = para $ \ c@(a :< f) -> cofree $ (alg (fmap (second (rhead . extract)) c) :. a) :< fmap snd f
