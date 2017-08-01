{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Ruby.Syntax
( assignment
, Syntax
, Grammar
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
  , Declaration.Function
  , Declaration.Method
  , Declaration.Module
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Expression.Call
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.MemberAccess
  , Expression.ScopeResolution
  , Expression.Subscript
  , Literal.Array
  , Literal.Boolean
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.String
  , Literal.Symbol
  , Literal.TextElement
  , Statement.Assignment
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
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Program
  , []
  ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment
assignment =
      makeTerm <$> symbol Program <*> children (Syntax.Program <$> many expression)
  <|> parseError

expression :: Assignment
expression =
      alias
  <|> assignment'
  <|> begin
  <|> beginBlock
  <|> binary
  <|> block
  <|> call
  <|> case'
  <|> class'
  <|> comment
  <|> conditional
  <|> emptyStatement
  <|> endBlock
  <|> for
  <|> heredoc
  <|> identifier
  <|> if'
  <|> keyword
  <|> lambda
  <|> literal
  <|> method
  <|> methodCall
  <|> mk Break Statement.Break
  <|> mk Next Statement.Continue
  <|> mk Redo Statement.Retry
  <|> mk Retry Statement.Retry
  <|> mk Return Statement.Return
  <|> mk Yield Statement.Yield
  <|> module'
  <|> pair
  <|> rescue
  <|> scopeResolution
  <|> singletonClass
  <|> singletonMethod
  <|> subscript
  <|> unary
  <|> undef
  <|> unless
  <|> until'
  <|> while'
  <|> parseError
  where mk s construct = makeTerm <$> symbol s <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children expression))

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

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
  <|> mk SplatArgument
  <|> mk HashSplatArgument
  <|> mk BlockArgument
  <|> mk ReservedIdentifier
  <|> mk Uninterpreted
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

literal :: Assignment
literal =
      makeTerm <$> symbol Grammar.True <*> (Literal.true <$ source)
  <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)
  <|> makeTerm <$> symbol Grammar.Integer <*> (Literal.Integer <$> source)
  <|> makeTerm <$> symbol Grammar.Float <*> (Literal.Float <$> source)
  <|> makeTerm <$> symbol Grammar.Nil <*> (Literal.Null <$ source)
   -- TODO: Do we want to represent the difference between .. and ...
  <|> makeTerm <$> symbol Range <*> children (Expression.Enumeration <$> expression <*> expression <*> emptyTerm)
  <|> makeTerm <$> symbol Array <*> children (Literal.Array <$> many expression)
  <|> makeTerm <$> symbol Hash <*> children (Literal.Hash <$> many pair)
  -- TODO: Give subshell it's own literal and allow interpolation
  <|> makeTerm <$> symbol Subshell <*> (Literal.TextElement <$> source)
  -- TODO: Handle interpolation
  <|> makeTerm <$> symbol String <*> (Literal.TextElement <$> source)
  -- TODO: this isn't quite right `"a" "b"` ends up as TextElement {textElementContent = "\"a\"\"b\""}
  <|> makeTerm <$> symbol ChainedString <*> children (Literal.TextElement . mconcat <$> many (symbol String *> source))
  -- TODO: Handle interpolation, dedicated literal?
  <|> makeTerm <$> symbol Regex <*> (Literal.TextElement <$> source)
  -- TODO: Handle interpolation
  <|> makeTerm <$> symbol Symbol <*> (Literal.Symbol <$> source)

heredoc :: Assignment
heredoc =  makeTerm <$> symbol HeredocBeginning <*> (Literal.TextElement <$> source)
       <|> makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

keyword :: Assignment
keyword =
      mk KeywordFILE
  <|> mk KeywordLINE
  <|> mk KeywordENCODING
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

beginBlock :: Assignment
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many expression)

endBlock :: Assignment
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many expression)

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> expression <*> (superclass <|> pure []) <*> many expression)
  where superclass = pure <$ symbol Superclass <*> children expression

singletonClass :: Assignment
singletonClass = makeTerm <$> symbol SingletonClass <*> children (Declaration.Class <$> expression <*> pure [] <*> many expression)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Declaration.Module <$> expression <*> many expression)

scopeResolution :: Assignment
scopeResolution = makeTerm <$> symbol ScopeResolution <*> children (Expression.ScopeResolution <$> many expression)

parameter :: Assignment
parameter =
      mk SplatParameter
  <|> mk HashSplatParameter
  <|> mk BlockParameter
  <|> mk KeywordParameter
  <|> mk OptionalParameter
  <|> makeTerm <$> symbol DestructuredParameter <*> children (many parameter)
  <|> expression
  <|> parseError
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

method :: Assignment
method = makeTerm <$> symbol Method <*> children (Declaration.Method <$> emptyTerm <*> expression <*> params <*> expressions)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

singletonMethod :: Assignment
singletonMethod = makeTerm <$> symbol SingletonMethod <*> children (Declaration.Method <$> expression <*> expression <*> params <*> expressions)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

lambda :: Assignment
lambda = symbol Lambda >>= \ loc -> children $ do
  name <- makeTerm loc <$> (Syntax.Identifier <$> source)
  params <- (symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure []
  body <- expressions
  pure $ makeTerm loc (Declaration.Function name params body)

block :: Assignment
block =  makeTerm <$> symbol DoBlock <*> children (Declaration.Function <$> emptyTerm <*> params <*> expressions)
     <|> makeTerm <$> symbol Block <*> children (Declaration.Function <$> emptyTerm <*> params <*> expressions)
  where params = (symbol BlockParameters) *> children (many parameter) <|> pure []

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment
alias = makeTerm <$> symbol Alias <*> children (Expression.Call <$> name <*> some expression <*> emptyTerm)
  where name = makeTerm <$> location <*> (Syntax.Identifier <$> source)

undef :: Assignment
undef = makeTerm <$> symbol Undef <*> children (Expression.Call <$> name <*> some expression <*> emptyTerm)
  where name = makeTerm <$> location <*> (Syntax.Identifier <$> source)

if' :: Assignment
if' =
      ifElsif If
  <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> expression <*> expression <*> emptyTerm)
  where ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> expression <*> expressions <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> else')))

else' :: Assignment
else' = makeTerm <$> symbol Else <*> children (many expression)

unless :: Assignment
unless =
      makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert expression <*> expressions <*> (fromMaybe <$> emptyTerm <*> optional else'))
  <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> expression <*> invert expression <*> emptyTerm)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> vars <*> expression <*> expressions)
  where vars = makeTerm <$> location <*> some expression

case' :: Assignment
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> expression <*> whens)
  where
    whens = makeTerm <$> location <*> many (when' <|> else' <|> expressions)
    when' = makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern) <*> whens)
    pattern = symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression)

subscript :: Assignment
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many expression)

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

methodCall :: Assignment
methodCall = makeTerm <$> symbol MethodCall <*> children (Expression.Call <$> expression <*> args <*> (block <|> emptyTerm))
  where
    args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many expression) <|> pure []


call :: Assignment
call = makeTerm <$> symbol Call <*> children (Expression.MemberAccess <$> expression <*> (expression <|> args))
  where
    args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (expressions)

rescue :: Assignment
rescue =  rescue'
      <|> makeTerm <$> symbol RescueModifier <*> children (Statement.Try <$> expression <*> many (makeTerm <$> location <*> (Statement.Catch <$> expression <*> emptyTerm)))
      <|> makeTerm <$> symbol Ensure <*> children (Statement.Finally <$> expressions)
      <|> makeTerm <$> symbol Else <*> children (Statement.Else <$> emptyTerm <*> expressions)
  where
    rescue' = makeTerm <$> symbol Rescue <*> children (Statement.Catch <$> exceptions <*> (rescue' <|> expressions))
    exceptions = makeTerm <$> location <*> many ex
    ex =  makeTerm <$> symbol Exceptions <*> children (many expression)
      <|> makeTerm <$> symbol ExceptionVariable <*> children (many expression)

begin :: Assignment
begin = makeTerm <$> symbol Begin <*> children (Statement.Try <$> expressions <*> many rescue)

assignment' :: Assignment
assignment'
   =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> lhs <*> rhs)
  <|> makeTerm <$> symbol OperatorAssignment <*> children (lhs >>= \ var -> Statement.Assignment var <$>
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
  where
    lhs = makeTerm <$> symbol LeftAssignmentList <*> children (many expr) <|> expr
    rhs = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr =
          makeTerm <$> symbol RestAssignment <*> (Syntax.Identifier <$> source)
      <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
      <|> expression

unary :: Assignment
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call <$> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier <$> source)) <*> some expression <*> emptyTerm)
  <|> children ( symbol AnonPlus *> expression )
  <|> makeTerm location . Expression.Negate <$> children expression -- Unary minus (e.g. `-a`). HiddenUnaryMinus nodes are hidden, so we can't match on the symbol.

binary  :: Assignment
binary = symbol Binary >>= \ loc -> children $ expression >>= \ lexpression -> go loc lexpression
  where
    go loc lexpression
       =  mk AnonAnd Expression.And
      <|> mk AnonAmpersandAmpersand Expression.And
      <|> mk AnonOr Expression.Or
      <|> mk AnonPipePipe Expression.Or
      <|> mk AnonLAngleLAngle Expression.LShift
      <|> mk AnonRAngleRAngle Expression.RShift
      <|> mk AnonEqualEqual Expression.Equal
      <|> mkNot AnonBangEqual Expression.Equal
       -- TODO: Distinguish `===` from `==` ?
      <|> mk AnonEqualEqualEqual Expression.Equal
      <|> mk AnonLAngleEqualRAngle Expression.Comparison
      -- TODO: Distinuish `=~` and `!~` ?
      <|> mk AnonEqualTilde Expression.Equal
      <|> mkNot AnonBangTilde Expression.Equal
      <|> mk AnonLAngle Expression.LessThan
      <|> mk AnonLAngleEqual Expression.LessThanEqual
      <|> mk AnonRAngle Expression.GreaterThan
      <|> mk AnonRAngleEqual Expression.GreaterThanEqual
      <|> mk AnonAmpersand Expression.BAnd
      <|> mk AnonCaret Expression.BXOr
      <|> mk AnonPipe Expression.BOr
      -- TODO: binary minus (hidden node). Doesn't work b/c we can't match hidden nodes (they aren't in the tree).
      -- <|> mk HiddenBinaryMinus Expression.Minus
      <|> mk AnonPlus Expression.Plus
      -- TODO: binary star (hidden node)
      <|> mk AnonSlash Expression.DividedBy
      <|> mk AnonPercent Expression.Modulo
      <|> mk AnonStarStar Expression.Power
      where mk s constr = makeTerm loc <$> (symbol s *> (constr lexpression <$> expression))
            mkNot s constr = makeTerm loc <$ symbol s <*> (Expression.Not <$> (makeTerm <$> location <*> (constr lexpression <$> expression)))

conditional :: Assignment
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> expression <*> expression <*> expression)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ source <|> pure Syntax.Empty)


-- Helper functions

makeTerm :: (f :< fs, HasCallStack) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree $ a :< inj f

emptyTerm :: Assignment
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty

invert :: (Expression.Boolean :< fs, HasCallStack) => Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location)) -> Assignment.Assignment ast grammar (Term.Term (Union fs) (Record Location))
invert term = makeTerm <$> location <*> fmap Expression.Not term

parseError :: Assignment
parseError = makeTerm <$> symbol ParseError <*> (Syntax.Error [] <$ source)
