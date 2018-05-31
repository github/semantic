{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Ruby.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Abstract.Name (name)
import Data.List (elem)
import Data.Record
import Data.Syntax (contextualize, postContextualize, emptyTerm, parseError, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1)
import Language.Ruby.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import Data.Sum
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Term as Term
import qualified Language.Ruby.Syntax as Ruby.Syntax
import Prologue hiding (for)

-- | The type of Ruby syntax.
type Syntax = '[
    Comment.Comment
  , Declaration.Function
  , Declaration.Method
  , Directive.File
  , Directive.Line
  , Expression.Arithmetic
  , Expression.Bitwise
  , Expression.Boolean
  , Expression.Call
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.Match
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
  , Literal.Regex
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
  , Ruby.Syntax.Class
  , Ruby.Syntax.Load
  , Ruby.Syntax.LowPrecedenceBoolean
  , Ruby.Syntax.Module
  , Ruby.Syntax.Require
  , Ruby.Syntax.Send
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment' a = HasCallStack => Assignment.Assignment [] Grammar a
type Assignment = Assignment' Term

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol Program <*> children (Syntax.Program <$> many expression) <|> parseError

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [ alias
  , assignment'
  , begin
  , beginBlock
  , binary
  , block
  , call
  , case'
  , class'
  , conditional
  , emptyStatement
  , endBlock
  , for
  , heredoc
  , identifier
  , if'
  , lambda
  , literal
  , method
  , methodCall
  , mk Break Statement.Break
  , mk Next Statement.Continue
  , mk Redo Statement.Retry
  , mk Retry Statement.Retry
  , mk Return Statement.Return
  , mk Yield Statement.Yield
  , module'
  , pair
  , parenthesizedExpressions
  , parseError
  , rescue
  , scopeResolution
  , singletonClass
  , singletonMethod
  , subscript
  , unary
  , undef
  , unless
  , until'
  , while'
  ]
  where
    mk s construct = makeTerm <$> symbol s <*> children ((construct .) . fromMaybe <$> emptyTerm <*> optional (symbol ArgumentList *> children expressions))

expressions :: Assignment
expressions = makeTerm'' <$> location <*> many expression

parenthesizedExpressions :: Assignment
parenthesizedExpressions = makeTerm'' <$> symbol ParenthesizedStatements <*> children (many expression)

withExtendedScope :: Assignment' a -> Assignment' a
withExtendedScope inner = do
  locals <- getRubyLocals
  result <- inner
  putRubyLocals locals
  pure result

withNewScope :: Assignment' a -> Assignment' a
withNewScope inner = withExtendedScope $ do
  putRubyLocals []
  inner

-- Looks up identifiers in the list of locals to determine vcall vs. local identifier.
identifier :: Assignment
identifier =
      vcallOrLocal
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
  <|> mk Uninterpreted
  where
    mk s = makeTerm <$> symbol s <*> (Syntax.Identifier . name <$> source)
    vcallOrLocal = do
      (loc, ident, locals) <- identWithLocals
      case ident of
        "__FILE__" -> pure $ makeTerm loc Directive.File
        "__LINE__" -> pure $ makeTerm loc Directive.Line
        _ -> do
          let identTerm = makeTerm loc (Syntax.Identifier (name ident))
          if ident `elem` locals
            then pure identTerm
            else pure $ makeTerm loc (Ruby.Syntax.Send Nothing (Just identTerm) [] Nothing)

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
  <|> makeTerm <$> symbol Hash  <*> children (Literal.Hash <$> many expression)
  <|> makeTerm <$> symbol Subshell <*> (Literal.TextElement <$> source)
  <|> makeTerm <$> symbol String <*> (Literal.TextElement <$> source)
  <|> makeTerm <$> symbol ChainedString <*> children (many (makeTerm <$> symbol String <*> (Literal.TextElement <$> source)))
  <|> makeTerm <$> symbol Regex <*> (Literal.Regex <$> source)
  <|> makeTerm <$> (symbol Symbol <|> symbol Symbol') <*> (Literal.Symbol <$> source)

heredoc :: Assignment
heredoc =  makeTerm <$> symbol HeredocBeginning <*> (Literal.TextElement <$> source)
       <|> makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

beginBlock :: Assignment
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many expression)

endBlock :: Assignment
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many expression)

class' :: Assignment
class' = makeTerm <$> symbol Class <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> optional superclass <*> expressions)
  where
    superclass :: Assignment
    superclass = symbol Superclass *> children expression

singletonClass :: Assignment
singletonClass = makeTerm <$> symbol SingletonClass <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> pure Nothing <*> expressions)

module' :: Assignment
module' = makeTerm <$> symbol Module <*> (withNewScope . children) (Ruby.Syntax.Module <$> expression <*> many expression)

scopeResolution :: Assignment
scopeResolution = makeTerm <$> symbol ScopeResolution <*> children (Expression.ScopeResolution <$> many expression)

parameter :: Assignment
parameter = postContextualize comment (term uncontextualizedParameter)
  where
    uncontextualizedParameter =
          lhsIdent
      <|> splatParameter
      <|> hashSplatParameter
      <|> blockParameter
      <|> keywordParameter
      <|> optionalParameter
      <|> makeTerm <$> symbol DestructuredParameter <*> children (many parameter)
    -- splat and hash splat arguments can be unnamed. we don't currently
    -- support unnamed arguments in the term syntax, so the use of emptyTerm
    -- here is a huge hack. what we should be able to do is return a Nothing
    -- for the argument name for splats and hash splats. TODO fix me:
    mkSplat s = symbol s *> children (lhsIdent <|> emptyTerm)
    splatParameter = mkSplat SplatParameter
    hashSplatParameter = mkSplat HashSplatParameter
    blockParameter = symbol BlockParameter *> children lhsIdent
    -- we don't yet care about default expressions for optional (including
    -- keyword) parameters, but we need to match on them to prevent errors:
    keywordParameter = symbol KeywordParameter *> children (lhsIdent <* optional expression)
    optionalParameter = symbol OptionalParameter *> children (lhsIdent <* expression)

method :: Assignment
method = makeTerm <$> symbol Method <*> (withNewScope . children) (Declaration.Method <$> pure [] <*> emptyTerm <*> methodSelector <*> params <*> expressions')
  where params = symbol MethodParameters *> children (many parameter) <|> pure []
        expressions' = makeTerm <$> location <*> many expression

singletonMethod :: Assignment
singletonMethod = makeTerm <$> symbol SingletonMethod <*> (withNewScope . children) (Declaration.Method <$> pure [] <*> expression <*> methodSelector <*> params <*> expressions)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

lambda :: Assignment
lambda = makeTerm <$> symbol Lambda <*> (withExtendedScope . children) (
  Declaration.Function [] <$> emptyTerm
                          <*> ((symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure [])
                          <*> expressions)

block :: Assignment
block =  makeTerm <$> symbol DoBlock <*> scopedBlockChildren
     <|> makeTerm <$> symbol Block <*> scopedBlockChildren
  where scopedBlockChildren = withExtendedScope blockChildren
        blockChildren = children (Declaration.Function <$> pure [] <*> emptyTerm <*> params <*> expressions)
        params = symbol BlockParameters *> children (many parameter) <|> pure []

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment
alias = makeTerm <$> symbol Alias <*> children (Expression.Call <$> pure [] <*> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

undef :: Assignment
undef = makeTerm <$> symbol Undef <*> children (Expression.Call <$> pure [] <*> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

if' :: Assignment
if' =   ifElsif If
    <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> expression <*> expression <*> emptyTerm)
  where
    ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> expression <*> expressions' <*> (elsif' <|> else' <|> emptyTerm))
    expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> void (symbol Elsif) <|> eof)
    elsif' = postContextualize comment (ifElsif Elsif)
    else' = postContextualize comment (symbol Else *> children expressions)

unless :: Assignment
unless =   makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert expression <*> expressions' <*> (else' <|> emptyTerm))
       <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> expression <*> invert expression <*> emptyTerm)
  where expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> eof)
        else' = postContextualize comment (symbol Else *> children expressions)

while' :: Assignment
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> (makeTerm <$> location <*> manyTermsTill expression (symbol In)) <*> inClause <*> expressions)
  where inClause = symbol In *> children expression

case' :: Assignment
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> (symbol When *> emptyTerm <|> expression) <*> whens)
  where
    whens = makeTerm <$> location <*> many (when' <|> else' <|> expression)
    when' = makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern') <*> whens)
    pattern' = postContextualize comment (symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression))
    else' = postContextualize comment (symbol Else *> children expressions)

subscript :: Assignment
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many expression)

pair :: Assignment
pair =   makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> (expression <|> emptyTerm))

args :: Assignment' [Term]
args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many expression) <|> many expression

methodCall :: Assignment
methodCall = makeTerm' <$> symbol MethodCall <*> children (require <|> load <|> send)
  where
    send = inject <$> ((regularCall <|> funcCall <|> scopeCall <|> dotCall) <*> optional block)

    funcCall = Ruby.Syntax.Send Nothing <$> selector <*> args
    regularCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> postContextualize heredoc expression) <*> selector) <*> args
    scopeCall = symbol ScopeResolution *> children (Ruby.Syntax.Send <$> (Just <$> expression) <*> selector) <*> args
    dotCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args)

    selector = Just <$> term methodSelector
    require = inject <$> (symbol Identifier *> do
      s <- source
      guard (s `elem` ["require", "require_relative"])
      Ruby.Syntax.Require (s == "require_relative") <$> nameExpression)
    load = inject <$> (symbol Identifier *> do
      s <- source
      guard (s == "load")
      Ruby.Syntax.Load <$> loadArgs)
    loadArgs = (symbol ArgumentList <|> symbol ArgumentListWithParens)  *> children (some expression)
    nameExpression = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children expression

methodSelector :: Assignment
methodSelector = makeTerm <$> symbols <*> (Syntax.Identifier <$> (name <$> source))
  where
    symbols = symbol Identifier
          <|> symbol Constant
          <|> symbol Operator
          <|> symbol Setter
          <|> symbol Super -- TODO(@charliesome): super calls are *not* method calls and need to be assigned into their own syntax terms

call :: Assignment
call = makeTerm <$> symbol Call <*> children (
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> (Just <$> methodSelector) <*> pure [] <*> pure Nothing) <|>
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args <*> pure Nothing))

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
assignment' = makeTerm  <$> symbol Assignment         <*> children (Statement.Assignment [] <$> lhs <*> rhs)
          <|> makeTerm' <$> symbol OperatorAssignment <*> children (infixTerm lhs expression
                [ assign Expression.Plus      <$ symbol AnonPlusEqual
                , assign Expression.Minus     <$ symbol AnonMinusEqual
                , assign Expression.Times     <$ symbol AnonStarEqual
                , assign Expression.Power     <$ symbol AnonStarStarEqual
                , assign Expression.DividedBy <$ symbol AnonSlashEqual
                , assign Expression.Or        <$ symbol AnonPipePipeEqual
                , assign Expression.BOr       <$ symbol AnonPipeEqual
                , assign Expression.And       <$ symbol AnonAmpersandAmpersandEqual
                , assign Expression.BAnd      <$ symbol AnonAmpersandEqual
                , assign Expression.Modulo    <$ symbol AnonPercentEqual
                , assign Expression.RShift    <$ symbol AnonRAngleRAngleEqual
                , assign Expression.LShift    <$ symbol AnonLAngleLAngleEqual
                , assign Expression.BXOr      <$ symbol AnonCaretEqual
                ])
  where
    assign :: (f :< Syntax) => (Term -> Term -> f Term) -> Term -> Term -> Sum Syntax Term
    assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))

    lhs  = makeTerm <$> symbol LeftAssignmentList  <*> children (many expr) <|> expr
    rhs  = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr = makeTerm <$> symbol RestAssignment      <*> (Syntax.Identifier . name <$> source)
       <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
       <|> lhsIdent
       <|> expression

identWithLocals :: Assignment' (Record Location, ByteString, [ByteString])
identWithLocals = do
  loc <- symbol Identifier
  -- source advances, so it's important we call getRubyLocals first
  locals <- getRubyLocals
  ident <- source
  pure (loc, ident, locals)

lhsIdent :: Assignment
lhsIdent = do
  (loc, ident, locals) <- identWithLocals
  putRubyLocals (ident : locals)
  pure $ makeTerm loc (Syntax.Identifier (name ident))

unary :: Assignment
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call <$> pure [] <*> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier . name <$> source)) <*> some expression <*> emptyTerm)
  <|> makeTerm location . Expression.Negate <$> children ( symbol AnonMinus' *> expression )
  <|> children ( symbol AnonPlus *> expression )

-- TODO: Distinguish `===` from `==` ?
binary :: Assignment
binary = makeTerm' <$> symbol Binary <*> children (infixTerm expression expression
  [ (inject .) . Expression.Plus             <$ symbol AnonPlus
  , (inject .) . Expression.Minus            <$ symbol AnonMinus'
  , (inject .) . Expression.Times            <$ symbol AnonStar'
  , (inject .) . Expression.Power            <$ symbol AnonStarStar
  , (inject .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inject .) . Expression.Modulo           <$ symbol AnonPercent
  , (inject .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inject .) . Ruby.Syntax.LowAnd          <$ symbol AnonAnd
  , (inject .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inject .) . Expression.Or               <$ symbol AnonPipePipe
  , (inject .) . Ruby.Syntax.LowOr           <$ symbol AnonOr
  , (inject .) . Expression.BOr              <$ symbol AnonPipe
  , (inject .) . Expression.BXOr             <$ symbol AnonCaret
  -- TODO: AnonEqualEqualEqual corresponds to Ruby's "case equality"
  -- function, which (unless overridden) is true if b is an instance
  -- of or inherits from a. We need a custom equality operator
  -- for this situation.
  , (inject .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual)
  , (inject .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (inject .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  , (inject .) . Expression.Comparison       <$ symbol AnonLAngleEqualRAngle
  , (inject .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inject .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inject .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inject .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (inject .) . Expression.Matches          <$ symbol AnonEqualTilde
  , (inject .) . Expression.NotMatches       <$ symbol AnonBangTilde
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
term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> heredocEnd) <*> emptyTerm)
  where heredocEnd = makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: HasCallStack
          => Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Sum Syntax Term)]
          -> Assignment.Assignment [] Grammar (Sum Syntax Term)
infixTerm = infixContext comment

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
