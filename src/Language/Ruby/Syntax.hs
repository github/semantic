{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Ruby.Syntax
( assignment
, Syntax
, Grammar
, Error
, Term
) where

import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Data.Union
import GHC.Stack
import Language.Ruby.Grammar as Grammar
import Prologue hiding (for, get, Location, state, unless)
import qualified Term

-- | The type of Ruby syntax.
type Syntax = '[
    Comment.Comment
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
  , Statement.Alias
  , Statement.Assignment
  , Statement.BeginBlock
  , Statement.Break
  , Statement.Continue
  , Statement.EndBlock
  , Statement.ForEach
  , Statement.If
  , Statement.Return
  , Statement.While
  , Statement.Yield
  , Statement.Undef
  , Syntax.Empty
  , Syntax.Error Error
  , Syntax.Identifier
  , []
  ]

type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment
assignment = makeTerm <$> symbol Program <*> children (many topLevelStatement)

topLevelStatement :: Assignment
topLevelStatement = handleError
   $  comment
  <|> beginBlock
  <|> endBlock
  <|> statement

statement :: Assignment
statement  = handleError
   $  alias
  <|> undef
  <|> emptyStatement
  -- <|> if'
  -- <|> unless
  --  $  exit Statement.Return Return
  -- <|> exit Statement.Yield Yield
  -- <|> exit Statement.Break Break
  -- <|> exit Statement.Continue Next
  -- <|> Language.Ruby.Syntax.while
  -- <|> until
  -- <|> for
  -- <|> literal
  -- <|> assignment'
  -- <|> class'
  -- <|> method
  -- where exit construct sym = makeTerm <$> symbol sym <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children statement))


beginBlock :: Assignment
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.BeginBlock <$> many topLevelStatement)

endBlock :: Assignment
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.EndBlock <$> many topLevelStatement)


-- class' :: Assignment
-- class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> (constant <|> scopeResolution) <*> (superclass <|> pure []) <*> many topLevelStatement)
--   where superclass = pure <$ symbol Superclass <*> children constant
--         scopeResolution = symbol ScopeResolution *> children (constant <|> identifier)

identifier :: Assignment
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

constant :: Assignment
constant = makeTerm <$> symbol Constant <*> (Syntax.Identifier <$> source)

variable :: Assignment
variable =  makeTerm <$> symbol InstanceVariable <*> (Syntax.Identifier <$> source)
        <|> makeTerm <$> symbol ClassVariable <*> (Syntax.Identifier <$> source)
        <|> makeTerm <$> symbol GlobalVariable <*> (Syntax.Identifier <$> source)

operator :: Assignment
operator = makeTerm <$> symbol Operator <*> (Syntax.Identifier <$> source)

symbol' :: Assignment
symbol' = makeTerm <$> symbol Symbol <*> (Literal.Symbol <$> source)

setter :: Assignment
setter = makeTerm <$> symbol Setter <*> (Syntax.Identifier <$> source)

methodName :: Assignment
methodName =  identifier
          <|> constant
          <|> variable
          <|> operator
          <|> symbol'
          <|> setter


-- method :: Assignment
-- method = makeTerm <$> symbol Method <*> children (Declaration.Method <$> identifier <*> pure [] <*> statements)

-- statements :: Assignment
-- statements = makeTerm <$> location <*> many statement

-- lvalue :: Assignment
-- lvalue = identifier

-- expression :: Assignment
-- expression = identifier <|> statement

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment
alias = makeTerm <$> symbol Alias <*> children (Statement.Alias <$> methodName <*> methodName)

undef :: Assignment
undef = makeTerm <$> symbol Undef <*> children (Statement.Undef <$> some methodName)

-- if' :: Assignment
-- if' =  ifElsif If
--    <|> makeTerm <$> symbol IfModifier     <*> children (flip Statement.If <$> statement <*> statement <*> (makeTerm <$> location <*> pure Syntax.Empty))
--   where ifElsif s = makeTerm <$> symbol s <*> children      (Statement.If <$> statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> makeTerm <$> symbol Else <*> children (many statement))))
--
-- unless :: Assignment
-- unless =  makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (makeTerm <$> symbol Else <*> children (many statement))))
--       <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> statement <*> invert statement <*> (makeTerm <$> location <*> pure Syntax.Empty))
--
-- while :: Assignment
-- while =  makeTerm <$> symbol While         <*> children      (Statement.While <$> statement <*> statements)
--      <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> statement <*> statement)
--
-- until :: Assignment
-- until =  makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert statement <*> statements)
--      <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> statement <*> invert statement)
--
-- for :: Assignment
-- for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> identifier <*> statement <*> statements)

-- assignment' :: Assignment
-- assignment'
--    =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> lvalue <*> expression)
--   <|> makeTerm <$> symbol OperatorAssignment <*> children (lvalue >>= \ var -> Statement.Assignment var <$>
--          (makeTerm <$> symbol AnonPlusEqual               <*> (Expression.Plus var      <$> expression)
--       <|> makeTerm <$> symbol AnonMinusEqual              <*> (Expression.Minus var     <$> expression)
--       <|> makeTerm <$> symbol AnonStarEqual               <*> (Expression.Times var     <$> expression)
--       <|> makeTerm <$> symbol AnonStarStarEqual           <*> (Expression.Power var     <$> expression)
--       <|> makeTerm <$> symbol AnonSlashEqual              <*> (Expression.DividedBy var <$> expression)
--       <|> makeTerm <$> symbol AnonPipePipeEqual           <*> (Expression.And var       <$> expression)
--       <|> makeTerm <$> symbol AnonPipeEqual               <*> (Expression.BOr var       <$> expression)
--       <|> makeTerm <$> symbol AnonAmpersandAmpersandEqual <*> (Expression.And var       <$> expression)
--       <|> makeTerm <$> symbol AnonAmpersandEqual          <*> (Expression.BAnd var      <$> expression)
--       <|> makeTerm <$> symbol AnonPercentEqual            <*> (Expression.Modulo var    <$> expression)
--       <|> makeTerm <$> symbol AnonRAngleRAngleEqual       <*> (Expression.RShift var    <$> expression)
--       <|> makeTerm <$> symbol AnonLAngleLAngleEqual       <*> (Expression.LShift var    <$> expression)
--       <|> makeTerm <$> symbol AnonCaretEqual              <*> (Expression.BXOr var      <$> expression)))

-- literal :: Assignment
-- literal  =  makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)
--         <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)
--         <|> makeTerm <$> symbol Grammar.Integer <*> (Literal.Integer <$> source)
--         <|> makeTerm <$> symbol Symbol <*> (Literal.Symbol <$> source)
--         <|> makeTerm <$> symbol Range <*> children (Literal.Range <$> statement <*> statement) -- FIXME: represent the difference between .. and ...

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> children (Syntax.Identifier <$> source)

--


makeTerm :: (f :< fs, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f

emptyTerm :: Assignment
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty

invert :: (Expression.Boolean :< fs, HasCallStack) => Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location)) -> Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location))
invert term = makeTerm <$> location <*> fmap Expression.Not term

handleError :: Assignment -> Assignment
handleError = flip catchError $ \ error -> case errorCause error of
  UnexpectedEndOfInput _ -> throwError error
  _ -> makeTerm <$> location <*> (Syntax.Error error <$ source)
