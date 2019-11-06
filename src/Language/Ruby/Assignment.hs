{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, RankNTypes, TypeFamilies, TypeOperators #-}
module Language.Ruby.Assignment
( assignment
, Ruby.Syntax
, Grammar
, Ruby.Term(..)
) where

import Prologue hiding (for, unless)

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (name)
import qualified Data.Abstract.ScopeGraph as ScopeGraph (AccessControl(..))
import           Data.List (elem)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Syntax
    ( contextualize
    , emptyTerm
    , handleError
    , infixContext
    , makeTerm
    , makeTerm'
    , makeTerm''
    , makeTerm1
    , parseError
    , postContextualize
    )
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Language.Ruby.Syntax as Ruby.Syntax
import           Language.Ruby.Term as Ruby
import           TreeSitter.Ruby as Grammar

type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment (Term Loc)
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> many expression) <|> parseError

expression :: Assignment (Term Loc)
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment (Term Loc)]
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
  , then'
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
  , self
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

expressions :: Assignment (Term Loc)
expressions = makeTerm'' <$> location <*> many expression

parenthesizedExpressions :: Assignment (Term Loc)
parenthesizedExpressions = makeTerm'' <$> symbol ParenthesizedStatements <*> children (many expression)

withExtendedScope :: Assignment a -> Assignment a
withExtendedScope inner = do
  locals <- getLocals
  result <- inner
  putLocals locals
  pure result

withNewScope :: Assignment a -> Assignment a
withNewScope inner = withExtendedScope $ do
  putLocals []
  inner

-- Looks up identifiers in the list of locals to determine vcall vs. local identifier.
identifier :: Assignment (Term Loc)
identifier =
      vcallOrLocal
  <|> zsuper
  <|> mk Constant
  <|> mk InstanceVariable
  <|> mk ClassVariable
  <|> mk GlobalVariable
  <|> mk Operator
  <|> mk Setter
  <|> mk SplatArgument
  <|> mk HashSplatArgument
  <|> mk BlockArgument
  <|> mk Uninterpreted
  where
    mk s = makeTerm <$> symbol s <*> (Syntax.Identifier . name <$> source)
    zsuper = makeTerm <$> symbol Super <*> (Ruby.Syntax.ZSuper <$ source)
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

self :: Assignment (Term Loc)
self = makeTerm <$> symbol Self <*> (Expression.This <$ source)

-- TODO: Handle interpolation in all literals that support it (strings, regexes, symbols, subshells, etc).
literal :: Assignment (Term Loc)
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
  <|> makeTerm <$> symbol StringArray <*> children (Literal.Array <$> many expression)
  <|> makeTerm <$> symbol SymbolArray <*> children (Literal.Array <$> many expression)
  <|> makeTerm <$> symbol Hash  <*> children (Literal.Hash <$> many expression)
  <|> makeTerm <$> symbol Subshell <*> (Literal.TextElement <$> source)
  <|> string
  <|> symbol'
  <|> makeTerm <$> symbol Character <*> (Literal.Character <$> source)
  <|> makeTerm <$> symbol ChainedString <*> children (many (makeTerm <$> symbol String <*> (Literal.TextElement <$> source)))
  <|> makeTerm <$> symbol Regex <*> (Literal.Regex <$> source)

  where
    string :: Assignment (Term Loc)
    string = makeTerm' <$> (symbol String <|> symbol BareString) <*>
      (children (inject . Literal.String <$> some (interpolation <|> escapeSequence)) <|> inject . Literal.TextElement <$> source)

    symbol' :: Assignment (Term Loc)
    symbol' = makeTerm' <$> (symbol Symbol <|> symbol Symbol' <|> symbol BareSymbol) <*>
      (children (inject . Literal.Symbol <$> some interpolation) <|> inject . Literal.SymbolElement <$> source)

interpolation :: Assignment (Term Loc)
interpolation = makeTerm <$> symbol Interpolation <*> children (Literal.InterpolationElement <$> expression)

escapeSequence :: Assignment (Term Loc)
escapeSequence = makeTerm <$> symbol EscapeSequence <*> (Literal.EscapeSequence <$> source)

heredoc :: Assignment (Term Loc)
heredoc =  makeTerm <$> symbol HeredocBeginning <*> (Literal.TextElement <$> source)
       <|> makeTerm <$> symbol HeredocBody <*> children (some (interpolation <|> escapeSequence <|> heredocEnd))
  where heredocEnd = makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

beginBlock :: Assignment (Term Loc)
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many expression)

endBlock :: Assignment (Term Loc)
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many expression)

class' :: Assignment (Term Loc)
class' = makeTerm <$> symbol Class <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> optional superclass <*> expressions)
  where
    superclass :: Assignment (Term Loc)
    superclass = symbol Superclass *> children expression

singletonClass :: Assignment (Term Loc)
singletonClass = makeTerm <$> symbol SingletonClass <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> pure Nothing <*> expressions)

module' :: Assignment (Term Loc)
module' = makeTerm <$> symbol Module <*> (withNewScope . children) (Ruby.Syntax.Module <$> expression <*> many expression)

scopeResolution :: Assignment (Term Loc)
scopeResolution = makeTerm <$> symbol ScopeResolution <*> children (Expression.ScopeResolution <$> NonEmpty.some1 expression)

parameter :: Assignment (Term Loc)
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

publicAccessControl :: ScopeGraph.AccessControl
publicAccessControl = ScopeGraph.Public

method :: Assignment (Term Loc)
method = makeTerm <$> symbol Method <*> (withNewScope . children) (Declaration.Method [] <$> emptyTerm <*> methodSelector <*> params <*> expressions' <*> pure publicAccessControl)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []
        expressions' = makeTerm <$> location <*> many expression

singletonMethod :: Assignment (Term Loc)
singletonMethod = makeTerm <$> symbol SingletonMethod <*> (withNewScope . children) (Declaration.Method [] <$> expression <*> methodSelector <*> params <*> expressions <*> pure publicAccessControl)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

lambda :: Assignment (Term Loc)
lambda = makeTerm <$> symbol Lambda <*> (withExtendedScope . children) (
  Declaration.Function [] <$> emptyTerm
                          <*> ((symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure [])
                          <*> expressions)

block :: Assignment (Term Loc)
block =  makeTerm <$> symbol DoBlock <*> scopedBlockChildren
     <|> makeTerm <$> symbol Block <*> scopedBlockChildren
  where scopedBlockChildren = withExtendedScope blockChildren
        blockChildren = children (Declaration.Function [] <$> emptyTerm <*> params <*> expressions)
        params = symbol BlockParameters *> children (many parameter) <|> pure []

comment :: Assignment (Term Loc)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment (Term Loc)
alias = makeTerm <$> symbol Alias <*> children (Expression.Call [] <$> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

undef :: Assignment (Term Loc)
undef = makeTerm <$> symbol Undef <*> children (Expression.Call [] <$> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

if' :: Assignment (Term Loc)
if' = ifElsif If
    <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> expression <*> expression <*> emptyTerm)
  where
    ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> expression <*> expressions' <*> (elsif' <|> else' <|> emptyTerm))
    elsif' = postContextualize comment (ifElsif Elsif)
    expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> void (symbol Elsif) <|> eof)
    else' = postContextualize comment (symbol Else *> children expressions)

then' :: Assignment (Term Loc)
then' = postContextualize comment (symbol Then *> children expressions)

unless :: Assignment (Term Loc)
unless =   makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert expression <*> expressions' <*> (else' <|> emptyTerm))
       <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> expression <*> invert expression <*> emptyTerm)
  where expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> eof)
        else' = postContextualize comment (symbol Else *> children expressions)

while' :: Assignment (Term Loc)
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment (Term Loc)
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment (Term Loc)
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> (makeTerm <$> location <*> manyTermsTill expression (symbol In)) <*> inClause <*> expressions)
  where inClause = symbol In *> children expression

case' :: Assignment (Term Loc)
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> (symbol When *> emptyTerm <|> expression) <*> whens)
  where
    whens = makeTerm <$> location <*> many (when' <|> else' <|> expression)
    when' = makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern') <*> whens)
    pattern' = postContextualize comment (symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression))
    else' = postContextualize comment (symbol Else *> children expressions)

subscript :: Assignment (Term Loc)
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many expression)

pair :: Assignment (Term Loc)
pair =   makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> (expression <|> emptyTerm))

args :: Assignment [Term Loc]
args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many expression) <|> many expression

methodCall :: Assignment (Term Loc)
methodCall = makeTerm' <$> symbol MethodCall <*> children (require <|> load <|> send)
  where
    send = inject <$> ((regularCall <|> funcCall <|> scopeCall <|> dotCall) <*> optional block)

    funcCall = Ruby.Syntax.Send Nothing <$> selector <*> args
    regularCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> postContextualize heredoc expression) <*> selector) <*> args
    scopeCall = symbol ScopeResolution *> children (Ruby.Syntax.Send <$> (Just <$> expression) <*> selector) <*> args
    dotCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args)

    selector = Just <$> term methodSelector
    require = inject <$> (symbol Identifier *> do
      s <- rawSource
      guard (s `elem` ["require", "require_relative"])
      Ruby.Syntax.Require (s == "require_relative") <$> nameExpression)
    load = inject <$ symbol Identifier <*> do
      s <- rawSource
      guard (s == "load")
      (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (Ruby.Syntax.Load <$> expression <*> optional expression)
    nameExpression = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children expression

methodSelector :: Assignment (Term Loc)
methodSelector = makeTerm <$> symbols <*> (Syntax.Identifier <$> (name <$> source))
  where
    symbols = symbol Identifier
          <|> symbol Constant
          <|> symbol Operator
          <|> symbol Setter
          <|> symbol Super -- TODO(@charliesome): super calls are *not* method calls and need to be assigned into their own syntax terms

call :: Assignment (Term Loc)
call = makeTerm <$> symbol Call <*> children (
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> (Just <$> methodSelector) <*> pure [] <*> pure Nothing) <|>
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args <*> pure Nothing))

rescue :: Assignment (Term Loc)
rescue =  rescue'
      <|> makeTerm <$> symbol RescueModifier <*> children (Statement.Try <$> expression <*> many (makeTerm <$> location <*> (Statement.Catch <$> expression <*> emptyTerm)))
      <|> makeTerm <$> symbol Ensure <*> children (Statement.Finally <$> expressions)
      <|> makeTerm <$> symbol Else <*> children (Statement.Else <$> emptyTerm <*> expressions)
  where
    rescue' = makeTerm <$> symbol Rescue <*> children (Statement.Catch <$> exceptions <*> (rescue' <|> expressions))
    exceptions = makeTerm <$> location <*> many ex
    ex =  makeTerm <$> symbol Exceptions <*> children (many expression)
      <|> makeTerm <$> symbol ExceptionVariable <*> children (many expression)

begin :: Assignment (Term Loc)
begin = makeTerm <$> symbol Begin <*> children (Statement.Try <$> expressions <*> many rescue)

assignment' :: Assignment (Term Loc)
assignment' = makeTerm  <$> symbol Assignment         <*> children (Ruby.Syntax.Assignment [] <$> lhs <*> rhs)
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
    assign :: (f :< Ruby.Syntax) => (Term Loc -> Term Loc -> f (Term Loc)) -> Term Loc -> Term Loc -> Sum Ruby.Syntax (Term Loc)
    assign c l r = inject (Ruby.Syntax.Assignment [] l (makeTerm1 (c l r)))

    lhs  = makeTerm <$> symbol LeftAssignmentList  <*> children (many expr) <|> expr
    rhs  = makeTerm <$> symbol RightAssignmentList <*> children (many expr) <|> expr
    expr = makeTerm <$> symbol RestAssignment      <*> (Syntax.Identifier . name <$> source)
       <|> makeTerm <$> symbol DestructuredLeftAssignment <*> children (many expr)
       <|> lhsIdent
       <|> expression

identWithLocals :: Assignment (Loc, Text, [Text])
identWithLocals = do
  loc <- symbol Identifier
  -- source advances, so it's important we call getLocals first
  locals <- getLocals
  ident <- source
  pure (loc, ident, locals)

lhsIdent :: Assignment (Term Loc)
lhsIdent = do
  (loc, ident, locals) <- identWithLocals
  putLocals (ident : locals)
  pure $ makeTerm loc (Syntax.Identifier (name ident))

unary :: Assignment (Term Loc)
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call [] <$> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier . name <$> source)) <*> some expression <*> emptyTerm)
  <|> makeTerm location . Expression.Negate <$> children ( (symbol AnonMinus <|> symbol AnonMinus' <|> symbol AnonMinus'') *> expression )
  <|> children ( symbol AnonPlus *> expression )

-- TODO: Distinguish `===` from `==` ?
binary :: Assignment (Term Loc)
binary = makeTerm' <$> symbol Binary <*> children (infixTerm expression expression
  [ (inject .) . Expression.Plus              <$ symbol AnonPlus
  , (inject .) . Expression.Minus             <$ (symbol AnonMinus <|> symbol AnonMinus' <|> symbol AnonMinus'')
  , (inject .) . Expression.Times             <$ (symbol AnonStar <|> symbol AnonStar')
  , (inject .) . Expression.Power             <$ symbol AnonStarStar
  , (inject .) . Expression.DividedBy         <$ (symbol AnonSlash <|> symbol AnonSlash')
  , (inject .) . Expression.Modulo            <$ symbol AnonPercent
  , (inject .) . Expression.And               <$ symbol AnonAmpersandAmpersand
  , (inject .) . Ruby.Syntax.LowPrecedenceAnd <$ symbol AnonAnd
  , (inject .) . Expression.BAnd              <$ symbol AnonAmpersand
  , (inject .) . Expression.Or                <$ symbol AnonPipePipe
  , (inject .) . Ruby.Syntax.LowPrecedenceOr  <$ symbol AnonOr
  , (inject .) . Expression.BOr               <$ symbol AnonPipe
  , (inject .) . Expression.BXOr              <$ symbol AnonCaret
  -- TODO: AnonEqualEqualEqual corresponds to Ruby's "case equality"
  -- function, which (unless overridden) is true if b is an instance
  -- of or inherits from a. We need a custom equality operator
  -- for this situation.
  , (inject .) . Expression.Equal            <$ (symbol AnonEqualEqual <|> symbol AnonEqualEqualEqual)
  , (inject .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (inject .) . Expression.LShift           <$ (symbol AnonLAngleLAngle <|> symbol AnonLAngleLAngle')
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

conditional :: Assignment (Term Loc)
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> expression <*> expression <*> expression)

emptyStatement :: Assignment (Term Loc)
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ rawSource <|> pure Syntax.Empty)


-- Helpers

invert :: Assignment (Term Loc) -> Assignment (Term Loc)
invert term = makeTerm <$> location <*> fmap Expression.Not term

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
term :: Assignment (Term Loc) -> Assignment (Term Loc)
term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> heredocEnd) <*> emptyTerm)
  where heredocEnd = makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment (Term Loc) -> Assignment b -> Assignment [Term Loc]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment (Term Loc)
          -> Assignment (Term Loc)
          -> [Assignment (Term Loc -> Term Loc -> Sum Ruby.Syntax (Term Loc))]
          -> Assignment (Sum Ruby.Syntax (Term Loc))
infixTerm = infixContext comment
