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
  , Expression.Comparison
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Integer
  , Literal.Null
  , Literal.Range
  , Literal.String
  , Literal.Symbol
  , Statement.Alias
  , Statement.Assignment
  , Statement.Break
  , Statement.Continue
  , Statement.ForEach
  , Statement.If
  , Statement.Return
  , Statement.ScopeEntry
  , Statement.ScopeExit
  , Statement.Undef
  , Statement.While
  , Statement.Yield
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
topLevelStatement = handleError $
      comment
  <|> beginBlock
  <|> endBlock
  <|> statement

statement :: Assignment
statement  = -- handleError $
      undef
  <|> alias
  <|> if'
  <|> unless
  <|> while'
  <|> until'
  <|> emptyStatement
  <|> mk Return Statement.Return
  <|> mk Yield Statement.Yield
  <|> mk Break Statement.Break
  <|> mk Next Statement.Continue
  <|> for
  <|> class'
  <|> method
  <|> identifier
  <|> literal
  <|> scopeResolution
  <|> conditional
  <|> unary
  <|> binary
  -- <|> assignment'
  -- TODO: rescue
  where mk s construct = makeTerm <$> symbol s <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children statement))

statements :: Assignment
statements = makeTerm <$> location <*> many statement


beginBlock :: Assignment
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many topLevelStatement)

endBlock :: Assignment
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many topLevelStatement)

identifier :: Assignment
identifier =
      mk Identifier
  <|> mk Constant
  <|> mk InstanceVariable
  <|> mk ClassVariable
  <|> mk GlobalVariable
  <|> mk Operator
  <|> mk Self
  <|> mk Super
  <|> mk Setter
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

literal :: Assignment
literal =
      makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)
  <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)
  <|> makeTerm <$> symbol Grammar.Integer <*> (Literal.Integer <$> source)
  <|> makeTerm <$> symbol Grammar.Nil <*> (Literal.Null <$ source)
  <|> makeTerm <$> symbol Symbol <*> (Literal.Symbol <$> source)
  <|> makeTerm <$> symbol Range <*> children (Literal.Range <$> statement <*> statement) -- TODO: Do we want to represent the difference between .. and ...

methodName :: Assignment
methodName = identifier <|> literal

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> (identifier <|> scopeResolution) <*> (superclass <|> pure []) <*> many topLevelStatement)
  where superclass = pure <$ symbol Superclass <*> children identifier

scopeResolution :: Assignment
scopeResolution = symbol ScopeResolution *> children identifier

method :: Assignment
method = makeTerm <$> symbol Method <*> children (Declaration.Method <$> methodName <*> pure [] <*> statements)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment
alias = makeTerm <$> symbol Alias <*> children (Statement.Alias <$> methodName <*> methodName)

undef :: Assignment
undef = makeTerm <$> symbol Undef <*> children (Statement.Undef <$> some methodName)

if' :: Assignment
if' =
      ifElsif If
  <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> statement <*> statement <*> emptyTerm)
  where ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> else')))
        else' = makeTerm <$> symbol Else <*> children (many statement)

unless :: Assignment
unless =
      makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional else'))
  <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> statement <*> invert statement <*> emptyTerm)
  where else' = makeTerm <$> symbol Else <*> children (many statement)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> statement <*> statements)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> statement <*> statement)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert statement <*> statements)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> statement <*> invert statement)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> identifier <*> statement <*> statements)

-- lvalue :: Assignment
-- lvalue = identifier

-- expression :: Assignment
-- expression = identifier <|> statement

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

unary :: Assignment
unary = symbol Unary >>= \ location ->
      -- TODO: Match a unary `defined?`
      -- makeTerm location . Expression.Call <$> children ( symbol AnonDefinedQuestion *> statement )
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> statement )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> statement )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> statement )
  <|> children ( symbol AnonPlus *> statement )
  -- FIXME: This also catches `defined? foo`, it shouldn't.
  <|> makeTerm location . Expression.Negate <$> children identifier -- Unary minus (e.g. `-a`). HiddenUnaryMinus nodes are hidden, so we can't match on the symbol.

binary  :: Assignment
binary = symbol Binary >>= \ loc -> children $ statement >>= \ lexpression -> go loc lexpression
  where
    go loc lexpression =
          mk AnonAnd Expression.And
      <|> mk AnonAmpersandAmpersand Expression.And
      <|> mk AnonOr Expression.Or
      <|> mk AnonPipePipe Expression.Or
      <|> mk AnonLAngleLAngle Expression.LShift
      <|> mk AnonRAngleRAngle Expression.RShift
      <|> mk AnonEqualEqual Expression.Equal
      <|> makeTerm loc <$ symbol AnonBangEqual <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> statement)))
       -- TODO: Distinguish `===` from `==` ?
      <|> mk AnonEqualEqualEqual Expression.Equal
      <|> mk AnonLAngleEqualRAngle Expression.Comparison
      -- TODO: Distinuish `=~` and `!~` ?
      <|> mk AnonEqualTilde Expression.Equal
      <|> makeTerm loc <$ symbol AnonBangTilde <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> statement)))
      <|> mk AnonLAngle Expression.LessThan
      <|> mk AnonLAngleEqual Expression.LessThanEqual
      <|> mk AnonRAngle Expression.GreaterThan
      <|> mk AnonRAngleEqual Expression.GreaterThanEqual
      <|> mk AnonAmpersand Expression.BAnd
      <|> mk AnonCaret Expression.BXOr
      <|> mk AnonPipe Expression.BOr
      -- TODO: binary minus (hidden node)
      <|> mk AnonPlus Expression.Plus
      -- TODO: binary star (hidden node)
      <|> mk AnonSlash Expression.DividedBy
      <|> mk AnonPercent Expression.Modulo
      <|> mk AnonStarStar Expression.Power
      where mk s constr = makeTerm loc <$> (symbol s *> (constr lexpression <$> statement))

conditional :: Assignment
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> statement <*> statement <*> statement)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$> (Just <$> source))


--
makeTerm :: (f :< fs, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f

emptyTerm :: Assignment
emptyTerm = makeTerm <$> location <*> pure (Syntax.Empty Nothing)

invert :: (Expression.Boolean :< fs, HasCallStack) => Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location)) -> Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location))
invert term = makeTerm <$> location <*> fmap Expression.Not term

handleError :: Assignment -> Assignment
handleError = flip catchError $ \ error -> case errorCause error of
  UnexpectedEndOfInput _ -> throwError error
  _ -> makeTerm <$> location <*> (Syntax.Error error <$ source)
