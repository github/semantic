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
  , Declaration.Function
  , Declaration.Method
  , Declaration.Module
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Expression.Call
  , Expression.Comparison
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
  , Literal.Range
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
  , Syntax.Error Error
  , Syntax.Identifier
  , []
  ]

type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment
assignment = makeTerm <$> symbol Program <*> children (many (handleError statement))

statement :: Assignment
statement = -- handleError $
      beginBlock
  <|> endBlock
  <|> comment
  <|> undef
  <|> alias
  <|> if'
  <|> unless
  <|> while'
  <|> until'
  <|> case'
  <|> emptyStatement
  <|> assignment'
  <|> unary
  <|> binary
  <|> literal
  <|> keyword
  <|> mk Return Statement.Return
  <|> mk Yield Statement.Yield
  <|> mk Break Statement.Break
  <|> mk Next Statement.Continue
  <|> mk Redo Statement.Retry
  <|> mk Retry Statement.Retry
  <|> for
  <|> class'
  <|> singletonClass
  <|> method
  <|> singletonMethod
  <|> lambda
  <|> module'
  <|> identifier
  <|> scopeResolution
  <|> conditional
  <|> methodCall
  <|> call
  <|> subscript
  <|> begin
  <|> rescue
  <|> block
  <|> heredoc
  where mk s construct = makeTerm <$> symbol s <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children statement))

statements :: Assignment
statements = makeTerm <$> location <*> many statement

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
  <|> makeTerm <$> symbol Range <*> children (Literal.Range <$> statement <*> statement)
  <|> makeTerm <$> symbol Array <*> children (Literal.Array <$> many statement)
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
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many statement)

endBlock :: Assignment
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many statement)

methodName :: Assignment
methodName = identifier <|> literal

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Declaration.Class <$> (identifier <|> scopeResolution) <*> (superclass <|> pure []) <*> many statement)
  where superclass = pure <$ symbol Superclass <*> children identifier

singletonClass :: Assignment
singletonClass = makeTerm <$> symbol SingletonClass <*> children (Declaration.Class <$> statement <*> pure [] <*> many statement)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Declaration.Module <$> (identifier <|> scopeResolution) <*> many statement)

scopeResolution :: Assignment
scopeResolution = makeTerm <$> symbol ScopeResolution <*> children (Expression.ScopeResolution <$> many statement)

parameter :: Assignment
parameter =
      mk SplatParameter
  <|> mk HashSplatParameter
  <|> mk BlockParameter
  <|> mk KeywordParameter
  <|> mk OptionalParameter
  <|> makeTerm <$> symbol DestructuredParameter <*> children (many parameter)
  <|> statement
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

method :: Assignment
method = makeTerm <$> symbol Method <*> children (Declaration.Method <$> emptyTerm <*> methodName <*> params <*> statements)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

singletonMethod :: Assignment
singletonMethod = makeTerm <$> symbol SingletonMethod <*> children (Declaration.Method <$> statement <*> methodName <*> params <*> statements)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

lambda :: Assignment
lambda = symbol Lambda >>= \ loc -> children $ do
  name <- makeTerm loc <$> (Syntax.Identifier <$> source)
  params <- (symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure []
  body <- statements
  pure $ makeTerm loc (Declaration.Function name params body)

block :: Assignment
block =  makeTerm <$> symbol DoBlock <*> children (Declaration.Function <$> emptyTerm <*> params <*> statements)
     <|> makeTerm <$> symbol Block <*> children (Declaration.Function <$> emptyTerm <*> params <*> statements)
  where params = (symbol BlockParameters) *> children (many parameter) <|> pure []

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment
alias = makeTerm <$> symbol Alias <*> children (Expression.Call <$> name <*> some methodName <*> emptyTerm)
  where name = makeTerm <$> location <*> (Syntax.Identifier <$> source)

undef :: Assignment
undef = makeTerm <$> symbol Undef <*> children (Expression.Call <$> name <*> some methodName <*> emptyTerm)
  where name = makeTerm <$> location <*> (Syntax.Identifier <$> source)

if' :: Assignment
if' =
      ifElsif If
  <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> statement <*> statement <*> emptyTerm)
  where ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> else')))

else' :: Assignment
else' = makeTerm <$> symbol Else <*> children (many statement)

unless :: Assignment
unless =
      makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert statement <*> statements <*> (fromMaybe <$> emptyTerm <*> optional else'))
  <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> statement <*> invert statement <*> emptyTerm)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> statement <*> statements)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> statement <*> statement)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert statement <*> statements)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> statement <*> invert statement)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> vars <*> statement <*> statements)
  where vars = makeTerm <$> location <*> some identifier

case' :: Assignment
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> statement <*> when)
  where
    when =  makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern) <*> (when <|> else' <|> statements))
    pattern = symbol Pattern *> children ((symbol SplatArgument *> children statement) <|> statement)

subscript :: Assignment
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> statement <*> many argument)

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> statement <*> statement)

argument :: Assignment
argument =
      mk SplatArgument
  <|> mk HashSplatArgument
  <|> mk BlockArgument
  <|> pair
  <|> statement
  where mk s = makeTerm <$> symbol s <*> (Syntax.Identifier <$> source)

methodCall :: Assignment
methodCall = makeTerm <$> symbol MethodCall <*> children (Expression.Call <$> name <*> args <*> (block <|> emptyTerm))
  where
    name = identifier <|> scopeResolution <|> call
    args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many argument) <|> pure []

call :: Assignment
call = makeTerm <$> symbol Call <*> children (Expression.MemberAccess <$> statement <*> statement)

rescue :: Assignment
rescue =  rescue'
      <|> makeTerm <$> symbol RescueModifier <*> children (Statement.Try <$> statement <*> many (makeTerm <$> location <*> (Statement.Catch <$> statement <*> emptyTerm)))
      <|> makeTerm <$> symbol Ensure <*> children (Statement.Finally <$> statements)
      <|> makeTerm <$> symbol Else <*> children (Statement.Else <$> emptyTerm <*> statements)
  where
    rescue' = makeTerm <$> symbol Rescue <*> children (Statement.Catch <$> exceptions <*> (rescue' <|> statements))
    exceptions = makeTerm <$> location <*> many ex
    ex =  makeTerm <$> symbol Exceptions <*> children (many identifier)
      <|> makeTerm <$> symbol ExceptionVariable <*> children (many identifier)

begin :: Assignment
begin = makeTerm <$> symbol Begin <*> children (Statement.Try <$> statements <*> many rescue)

assignment' :: Assignment
assignment'
   =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> lhs <*> rhs)
  <|> makeTerm <$> symbol OperatorAssignment <*> children (lhs >>= \ var -> Statement.Assignment var <$>
         (makeTerm <$> symbol AnonPlusEqual               <*> (Expression.Plus var      <$> statement)
      <|> makeTerm <$> symbol AnonMinusEqual              <*> (Expression.Minus var     <$> statement)
      <|> makeTerm <$> symbol AnonStarEqual               <*> (Expression.Times var     <$> statement)
      <|> makeTerm <$> symbol AnonStarStarEqual           <*> (Expression.Power var     <$> statement)
      <|> makeTerm <$> symbol AnonSlashEqual              <*> (Expression.DividedBy var <$> statement)
      <|> makeTerm <$> symbol AnonPipePipeEqual           <*> (Expression.And var       <$> statement)
      <|> makeTerm <$> symbol AnonPipeEqual               <*> (Expression.BOr var       <$> statement)
      <|> makeTerm <$> symbol AnonAmpersandAmpersandEqual <*> (Expression.And var       <$> statement)
      <|> makeTerm <$> symbol AnonAmpersandEqual          <*> (Expression.BAnd var      <$> statement)
      <|> makeTerm <$> symbol AnonPercentEqual            <*> (Expression.Modulo var    <$> statement)
      <|> makeTerm <$> symbol AnonRAngleRAngleEqual       <*> (Expression.RShift var    <$> statement)
      <|> makeTerm <$> symbol AnonLAngleLAngleEqual       <*> (Expression.LShift var    <$> statement)
      <|> makeTerm <$> symbol AnonCaretEqual              <*> (Expression.BXOr var      <$> statement)))
  where
    lhs = makeTerm <$> symbol LeftAssignmentList <*> children (many expr) <|> expr
    rhs = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr =
          makeTerm <$> symbol RestAssignment <*> (Syntax.Identifier <$> source)
      <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
      <|> argument


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
      -- TODO: binary minus (hidden node)
      <|> mk AnonPlus Expression.Plus
      -- TODO: binary star (hidden node)
      <|> mk AnonSlash Expression.DividedBy
      <|> mk AnonPercent Expression.Modulo
      <|> mk AnonStarStar Expression.Power
      where mk s constr = makeTerm loc <$> (symbol s *> (constr lexpression <$> statement))
            mkNot s constr = makeTerm loc <$ symbol s <*> (Expression.Not <$> (makeTerm <$> location <*> (constr lexpression <$> statement)))

conditional :: Assignment
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> statement <*> statement <*> statement)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$> (Just <$> source))


-- Helper functions

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
