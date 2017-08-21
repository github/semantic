{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Ruby.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Maybe (fromMaybe)
import Data.Record
import Data.Functor (void)
import Data.List.NonEmpty (some1)
import Data.Syntax (contextualize, emptyTerm, parseError, handleError, infixContext, makeTerm, makeTerm', makeTerm1)
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
  , Literal.Complex
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.KeyValue
  , Literal.Null
  , Literal.Rational
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
  , Syntax.Context
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
assignment = handleError $ makeTerm <$> symbol Program <*> children (Syntax.Program <$> many expression)

expression :: Assignment
expression = handleError . term $
      alias
  <|> assignment'
  <|> begin
  <|> beginBlock
  <|> binary
  <|> block
  <|> call
  <|> case'
  <|> class'
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
  <|> parenthesized_expressions
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
  where mk s construct = makeTerm <$> symbol s <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children expressions))

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

parenthesized_expressions :: Assignment
parenthesized_expressions = makeTerm <$> symbol ParenthesizedStatements <*> children (many expression)

identifier :: Assignment
identifier =
      mk Identifier
  <|> mk Identifier'
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

-- TODO: Handle interpolation in all literals that support it (strings, regexes, symbols, subshells, etc).
literal :: Assignment
literal =
      makeTerm <$> token  Grammar.True     <*> pure Literal.true
  <|> makeTerm <$> token  Grammar.False    <*> pure Literal.false
  <|> makeTerm <$> token  Grammar.Nil      <*> pure Literal.Null
  <|> makeTerm <$> symbol Grammar.Integer  <*> (Literal.Integer <$> source)
  <|> makeTerm <$> symbol Grammar.Float    <*> (Literal.Float <$> source)
  <|> makeTerm <$> symbol Grammar.Rational <*> (Literal.Rational <$> source)
  <|> makeTerm <$> symbol Grammar.Complex  <*> (Literal.Complex <$> source)
   -- TODO: Do we want to represent the difference between .. and ...
  <|> makeTerm <$> symbol Range <*> children (Expression.Enumeration <$> expression <*> expression <*> emptyTerm)
  <|> makeTerm <$> symbol Array <*> children (Literal.Array <$> many expression)
  <|> makeTerm <$> symbol Hash  <*> children (Literal.Hash <$> (many . term) pair)
  <|> makeTerm <$> symbol Subshell <*> (Literal.TextElement <$> source)
  <|> makeTerm <$> symbol String <*> (Literal.TextElement <$> source)
  <|> makeTerm <$> symbol ChainedString <*> children (many (term (makeTerm <$> symbol String <*> (Literal.TextElement <$> source))))
  <|> makeTerm <$> symbol Regex <*> (Literal.TextElement <$> source)
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
if' =  ifElsif If
   <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> expression <*> expression <*> emptyTerm)
  where
    ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> expression <*> expressions' <*> (fromMaybe <$> emptyTerm <*> optional (ifElsif Elsif <|> else')))
    expressions' = makeTerm <$> location <*> manyTill expression (void (symbol Else) <|> void (symbol Elsif) <|> eof)

else' :: Assignment
else' = makeTerm <$> symbol Else <*> children (many expression)

unless :: Assignment
unless =
      makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert expression <*> expressions' <*> (fromMaybe <$> emptyTerm <*> optional else'))
  <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> expression <*> invert expression <*> emptyTerm)
  where expressions' = makeTerm <$> location <*> manyTill expression (void (symbol Else) <|> eof)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> (makeTerm <$> location <*> manyTill expression (symbol In)) <*> inClause <*> expressions)
  where inClause = symbol In *> children (expression)

case' :: Assignment
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> (symbol When *> emptyTerm <|> expression) <*> whens)
  where
    whens = makeTerm <$> location <*> many (when' <|> else' <|> expression)
    when' = makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern) <*> whens)
    pattern = symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression)

subscript :: Assignment
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many expression)

pair :: Assignment
pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)
   <|> makeTerm <$> token  Pair <*> pure Syntax.Empty

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
assignment' = makeTerm  <$> symbol Assignment         <*> children (Statement.Assignment <$> lhs <*> rhs)
          <|> makeTerm' <$> symbol OperatorAssignment <*> children (infixTerm lhs expression
                [ assign Expression.Plus      <$ symbol AnonPlusEqual
                , assign Expression.Minus     <$ symbol AnonMinusEqual
                , assign Expression.Times     <$ symbol AnonStarEqual
                , assign Expression.Power     <$ symbol AnonStarStarEqual
                , assign Expression.DividedBy <$ symbol AnonSlashEqual
                , assign Expression.And       <$ symbol AnonPipePipeEqual
                , assign Expression.BOr       <$ symbol AnonPipeEqual
                , assign Expression.And       <$ symbol AnonAmpersandAmpersandEqual
                , assign Expression.BAnd      <$ symbol AnonAmpersandEqual
                , assign Expression.Modulo    <$ symbol AnonPercentEqual
                , assign Expression.RShift    <$ symbol AnonRAngleRAngleEqual
                , assign Expression.LShift    <$ symbol AnonLAngleLAngleEqual
                , assign Expression.BXOr      <$ symbol AnonCaretEqual
                ])
  where
    assign :: f :< Syntax => (Term -> Term -> f Term) -> Term -> Term -> Union Syntax Term
    assign c l r = inj (Statement.Assignment l (makeTerm1 (c l r)))

    lhs  = makeTerm <$> symbol LeftAssignmentList  <*> children (many expr) <|> expr
    rhs  = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr = makeTerm <$> symbol RestAssignment      <*> (Syntax.Identifier <$> source)
       <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
       <|> expression

unary :: Assignment
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call <$> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier <$> source)) <*> some expression <*> emptyTerm)
  <|> makeTerm location . Expression.Negate <$> children ( symbol AnonMinus' *> expression )
  <|> children ( symbol AnonPlus *> expression )

-- TODO: Distinguish `===` from `==` ?
-- TODO: Distinuish `=~` and `!~` ?
binary  :: Assignment
binary = makeTerm' <$> symbol Binary <*> children (infixTerm expression expression
  [ (inj .) . Expression.Plus             <$ symbol AnonPlus
  , (inj .) . Expression.Minus            <$ symbol AnonMinus'
  , (inj .) . Expression.Times            <$ symbol AnonStar'
  , (inj .) . Expression.Power            <$ symbol AnonStarStar
  , (inj .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inj .) . Expression.Modulo           <$ symbol AnonPercent
  , (inj .) . Expression.And              <$ (symbol AnonAnd <|> symbol AnonAmpersandAmpersand)
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inj .) . Expression.Or               <$ (symbol AnonOr <|> symbol AnonPipePipe)
  , (inj .) . Expression.BOr              <$ symbol AnonPipe
  , (inj .) . Expression.BXOr             <$ symbol AnonCaret
  , (inj .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual <|> symbol AnonEqualTilde)
  , (inj .) . invert Expression.Equal     <$ (symbol AnonBangEqual <|> symbol AnonBangTilde)
  , (inj .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inj .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inj .) . Expression.Comparison       <$ symbol AnonLAngleEqualRAngle
  , (inj .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inj .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inj .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inj .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

conditional :: Assignment
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> expression <*> expression <*> expression)

emptyStatement :: Assignment
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ source <|> pure Syntax.Empty)


-- Helper functions

invert :: Assignment -> Assignment
invert term = makeTerm <$> location <*> fmap Expression.Not term

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
term :: Assignment -> Assignment
term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: HasCallStack
          => Assignment
          -> Assignment
          -> [Assignment.Assignment (AST Grammar) Grammar (Term -> Term -> Union Syntax Term)]
          -> Assignment.Assignment (AST Grammar) Grammar (Union Syntax Term)
infixTerm = infixContext comment
