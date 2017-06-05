{-# LANGUAGE DataKinds #-}
module Language.Ruby.Syntax
( assignment
, Syntax
, Syntax'
, Grammar
, Error
) where

import Data.Functor.Union
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Error)
import qualified Data.Syntax.Assignment as Assignment
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
  , Syntax.Error Error
  , Syntax.Identifier
  , []
  ]

type Error = Assignment.Error Grammar


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
assignment = makeTerm <$> symbol Program <*> children (many declaration)

declaration :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location)
declaration = handleError $ comment <|> class' <|> method

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
statement  = handleError
           $  exit Statement.Return Return
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

handleError :: HasCallStack => Assignment (Node Grammar) (Term Syntax Location) -> Assignment (Node Grammar) (Term Syntax Location)
handleError = flip catchError $ \ error -> case errorCause error of
  UnexpectedEndOfInput _ -> throwError error
  _ -> makeTerm <$> location <*> (Syntax.Error error <$ source)
