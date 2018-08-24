{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Ruby.Assignment
( assignment
, Syntax
, Grammar
, Term

-- Small version of Ruby to enable internal framework development.
, miniAssignment
, MiniSyntax
, MiniTerm
) where

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (name)
import           Data.List (elem)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Record
import           Data.Sum
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
import qualified Data.Term as Term
import qualified Data.Diff as Diff
import           Language.Ruby.Grammar as Grammar
import qualified Language.Ruby.Syntax as Ruby.Syntax
import           Prologue hiding (for)
import           Proto3.Suite (Named (..), Named1 (..))

-- TODO: Only needed for as long as we carry around a mini ruby syntax.
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | Small version of Ruby syntax for testing the code rewriting pipeline.
type MiniSyntax = '[
    Literal.Integer
  , Comment.Comment
  , Declaration.Method
  , Declaration.Function
  -- , Expression.Call
  , Expression.Plus
  -- , Expression.Minus
  -- , Expression.Times
  , Ruby.Syntax.Send
  -- , Ruby.Syntax.Load
  -- , Ruby.Syntax.Require
  , Statement.Statements
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , []
  ]

type MiniTerm = Term.Term (Sum MiniSyntax) (Record Location)

miniAssignment :: Assignment MiniTerm
miniAssignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> many expression) <|> parseError
  where
    expression :: Assignment MiniTerm
    expression = term . handleError $
      choice [ binary
             , identifier
             , number
             , method
             , methodCall ]

    -- NOTE: Important that we don't flatten out the Imperative for single item lists
    expressions :: Assignment MiniTerm
    expressions = makeTerm <$> location <*> many expression

    number :: Assignment MiniTerm
    number = makeTerm <$> symbol Grammar.Integer <*> (Literal.Integer <$> source)

    identifier :: Assignment MiniTerm
    identifier =
          vcallOrLocal
      <|> mk Constant
      <|> mk InstanceVariable
      <|> mk ClassVariable
      <|> mk GlobalVariable
      <|> mk Operator
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
          let identTerm = makeTerm loc (Syntax.Identifier (name ident))
          if ident `elem` locals
            then pure identTerm
            else pure $ makeTerm loc (Ruby.Syntax.Send Nothing (Just identTerm) [] Nothing)

    method :: Assignment MiniTerm
    method = makeTerm <$> symbol Method <*> (withNewScope . children) (Declaration.Method [] <$> emptyTerm <*> methodSelector <*> params <*> expressions')
      where params = symbol MethodParameters *> children (many parameter) <|> pure []
            expressions' = makeTerm <$> location <*> many expression

    methodSelector :: Assignment MiniTerm
    methodSelector = makeTerm <$> symbols <*> (Syntax.Identifier <$> (name <$> source))
      where
        symbols = symbol Identifier
              <|> symbol Constant
              <|> symbol Operator
              <|> symbol Setter
              <|> symbol Super -- TODO(@charliesome): super calls are *not* method calls and need to be assigned into their own syntax terms

    parameter :: Assignment MiniTerm
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

    lhsIdent :: Assignment MiniTerm
    lhsIdent = do
      (loc, ident, locals) <- identWithLocals
      putLocals (ident : locals)
      pure $ makeTerm loc (Syntax.Identifier (name ident))

    methodCall :: Assignment MiniTerm
    methodCall = makeTerm' <$> symbol MethodCall <*> children send -- (require <|> load <|> send)
      where
        send = inject <$> ((regularCall <|> funcCall <|> scopeCall <|> dotCall) <*> optional block)

        funcCall = Ruby.Syntax.Send Nothing <$> selector <*> args
        regularCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> expression) <*> selector) <*> args
        scopeCall = symbol ScopeResolution *> children (Ruby.Syntax.Send <$> (Just <$> expression) <*> selector) <*> args
        dotCall = symbol Call *> children (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args)

        selector = Just <$> term methodSelector
        -- require = inject <$> (symbol Identifier *> do
        --   s <- rawSource
        --   guard (s `elem` ["require", "require_relative"])
        --   Ruby.Syntax.Require (s == "require_relative") <$> nameExpression)
        -- load = inject <$ symbol Identifier <*> do
        --   s <- rawSource
        --   guard (s == "load")
        --   (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (Ruby.Syntax.Load <$> expression <*> optional expression)
        -- nameExpression = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children expression

    args :: Assignment [MiniTerm]
    args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many expression) <|> many expression

    block :: Assignment MiniTerm
    block =  makeTerm <$> symbol DoBlock <*> scopedBlockChildren
         <|> makeTerm <$> symbol Block <*> scopedBlockChildren
      where scopedBlockChildren = withExtendedScope blockChildren
            blockChildren = children (Declaration.Function [] <$> emptyTerm <*> params <*> expressions)
            params = symbol BlockParameters *> children (many parameter) <|> pure []

    binary :: Assignment MiniTerm
    binary = makeTerm' <$> symbol Binary <*> children (infixTerm expression expression
      [ (inject .) . Expression.Plus              <$ symbol AnonPlus
      -- , (inject .) . Expression.Minus             <$ symbol AnonMinus'
      -- , (inject .) . Expression.Times             <$ symbol AnonStar'
      ])

    comment :: Assignment MiniTerm
    comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

    term :: Assignment MiniTerm -> Assignment MiniTerm
    term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)

    -- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
    infixTerm :: Assignment MiniTerm
              -> Assignment MiniTerm
              -> [Assignment (MiniTerm -> MiniTerm -> Sum MiniSyntax MiniTerm)]
              -> Assignment (Sum MiniSyntax MiniTerm)
    infixTerm = infixContext comment


-- | The type of Ruby syntax.
type Syntax = '[
    Comment.Comment
  , Declaration.Function
  , Declaration.Method
  , Directive.File
  , Directive.Line
  , Expression.Plus
  , Expression.Minus
  , Expression.Times
  , Expression.DividedBy
  , Expression.Modulo
  , Expression.Power
  , Expression.Negate
  , Expression.FloorDivision
  , Expression.BAnd
  , Expression.BOr
  , Expression.BXOr
  , Expression.LShift
  , Expression.RShift
  , Expression.Complement
  , Expression.And
  , Expression.Not
  , Expression.Or
  , Expression.XOr
  , Expression.Call
  , Expression.LessThan
  , Expression.LessThanEqual
  , Expression.GreaterThan
  , Expression.GreaterThanEqual
  , Expression.Equal
  , Expression.StrictEqual
  , Expression.Comparison
  , Expression.Enumeration
  , Expression.Matches
  , Expression.NotMatches
  , Expression.MemberAccess
  , Expression.ScopeResolution
  , Expression.Subscript
  , Expression.Member
  , Expression.This
  , Literal.Array
  , Literal.Boolean
  , Literal.Character
  , Literal.Complex
  , Literal.EscapeSequence
  , Literal.Float
  , Literal.Hash
  , Literal.Integer
  , Literal.InterpolationElement
  , Literal.KeyValue
  , Literal.Null
  , Literal.Rational
  , Literal.Regex
  , Literal.String
  , Literal.Symbol
  , Literal.SymbolElement
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
  , Statement.Statements
  , Statement.Try
  , Statement.While
  , Statement.Yield
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Ruby.Syntax.Class
  , Ruby.Syntax.Load
  , Ruby.Syntax.LowPrecedenceAnd
  , Ruby.Syntax.LowPrecedenceOr
  , Ruby.Syntax.Module
  , Ruby.Syntax.Require
  , Ruby.Syntax.Send
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar

instance Named1 (Sum Syntax) where
  nameOf1 _ = "RubySyntax"

instance Named (Term.Term (Sum Syntax) ()) where
  nameOf _ = "RubyTerm"

instance Named (Diff.Diff (Sum Syntax) () ()) where
  nameOf _ = "RubyDiff"

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Term
assignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> many expression) <|> parseError

expression :: Assignment Term
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment Term]
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

expressions :: Assignment Term
expressions = makeTerm'' <$> location <*> many expression

parenthesizedExpressions :: Assignment Term
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
identifier :: Assignment Term
identifier =
      vcallOrLocal
  <|> mk Constant
  <|> mk InstanceVariable
  <|> mk ClassVariable
  <|> mk GlobalVariable
  <|> mk Operator
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

self :: Assignment Term
self = makeTerm <$> symbol Self <*> (Expression.This <$ source)

-- TODO: Handle interpolation in all literals that support it (strings, regexes, symbols, subshells, etc).
literal :: Assignment Term
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
    string :: Assignment Term
    string = makeTerm' <$> (symbol String <|> symbol BareString) <*>
      (children (inject . Literal.String <$> some (interpolation <|> escapeSequence)) <|> inject . Literal.TextElement <$> source)

    symbol' :: Assignment Term
    symbol' = makeTerm' <$> (symbol Symbol <|> symbol Symbol' <|> symbol BareSymbol) <*>
      (children (inject . Literal.Symbol <$> some interpolation) <|> inject . Literal.SymbolElement <$> source)

interpolation :: Assignment Term
interpolation = makeTerm <$> symbol Interpolation <*> children (Literal.InterpolationElement <$> expression)

escapeSequence :: Assignment Term
escapeSequence = makeTerm <$> symbol EscapeSequence <*> (Literal.EscapeSequence <$> source)

heredoc :: Assignment Term
heredoc =  makeTerm <$> symbol HeredocBeginning <*> (Literal.TextElement <$> source)
       <|> makeTerm <$> symbol HeredocBody <*> children (some (interpolation <|> escapeSequence <|> heredocEnd))
  where heredocEnd = makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

beginBlock :: Assignment Term
beginBlock = makeTerm <$> symbol BeginBlock <*> children (Statement.ScopeEntry <$> many expression)

endBlock :: Assignment Term
endBlock = makeTerm <$> symbol EndBlock <*> children (Statement.ScopeExit <$> many expression)

class' :: Assignment Term
class' = makeTerm <$> symbol Class <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> optional superclass <*> expressions)
  where
    superclass :: Assignment Term
    superclass = symbol Superclass *> children expression

singletonClass :: Assignment Term
singletonClass = makeTerm <$> symbol SingletonClass <*> (withNewScope . children) (Ruby.Syntax.Class <$> expression <*> pure Nothing <*> expressions)

module' :: Assignment Term
module' = makeTerm <$> symbol Module <*> (withNewScope . children) (Ruby.Syntax.Module <$> expression <*> many expression)

scopeResolution :: Assignment Term
scopeResolution = makeTerm <$> symbol ScopeResolution <*> children (Expression.ScopeResolution <$> NonEmpty.some1 expression)

parameter :: Assignment Term
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

method :: Assignment Term
method = makeTerm <$> symbol Method <*> (withNewScope . children) (Declaration.Method [] <$> emptyTerm <*> methodSelector <*> params <*> expressions')
  where params = symbol MethodParameters *> children (many parameter) <|> pure []
        expressions' = makeTerm <$> location <*> many expression

singletonMethod :: Assignment Term
singletonMethod = makeTerm <$> symbol SingletonMethod <*> (withNewScope . children) (Declaration.Method [] <$> expression <*> methodSelector <*> params <*> expressions)
  where params = symbol MethodParameters *> children (many parameter) <|> pure []

lambda :: Assignment Term
lambda = makeTerm <$> symbol Lambda <*> (withExtendedScope . children) (
  Declaration.Function [] <$> emptyTerm
                          <*> ((symbol BlockParameters <|> symbol LambdaParameters) *> children (many parameter) <|> pure [])
                          <*> expressions)

block :: Assignment Term
block =  makeTerm <$> symbol DoBlock <*> scopedBlockChildren
     <|> makeTerm <$> symbol Block <*> scopedBlockChildren
  where scopedBlockChildren = withExtendedScope blockChildren
        blockChildren = children (Declaration.Function [] <$> emptyTerm <*> params <*> expressions)
        params = symbol BlockParameters *> children (many parameter) <|> pure []

comment :: Assignment Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

alias :: Assignment Term
alias = makeTerm <$> symbol Alias <*> children (Expression.Call [] <$> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

undef :: Assignment Term
undef = makeTerm <$> symbol Undef <*> children (Expression.Call [] <$> name' <*> some expression <*> emptyTerm)
  where name' = makeTerm <$> location <*> (Syntax.Identifier . name <$> source)

if' :: Assignment Term
if' =   ifElsif If
    <|> makeTerm <$> symbol IfModifier <*> children (flip Statement.If <$> expression <*> expression <*> emptyTerm)
  where
    ifElsif s = makeTerm <$> symbol s <*> children (Statement.If <$> expression <*> expressions' <*> (elsif' <|> else' <|> emptyTerm))
    expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> void (symbol Elsif) <|> eof)
    elsif' = postContextualize comment (ifElsif Elsif)
    else' = postContextualize comment (symbol Else *> children expressions)

unless :: Assignment Term
unless =   makeTerm <$> symbol Unless         <*> children      (Statement.If <$> invert expression <*> expressions' <*> (else' <|> emptyTerm))
       <|> makeTerm <$> symbol UnlessModifier <*> children (flip Statement.If <$> expression <*> invert expression <*> emptyTerm)
  where expressions' = makeTerm <$> location <*> manyTermsTill expression (void (symbol Else) <|> eof)
        else' = postContextualize comment (symbol Else *> children expressions)

while' :: Assignment Term
while' =
      makeTerm <$> symbol While         <*> children      (Statement.While <$> expression <*> expressions)
  <|> makeTerm <$> symbol WhileModifier <*> children (flip Statement.While <$> expression <*> expression)

until' :: Assignment Term
until' =
      makeTerm <$> symbol Until         <*> children      (Statement.While <$> invert expression <*> expressions)
  <|> makeTerm <$> symbol UntilModifier <*> children (flip Statement.While <$> expression <*> invert expression)

for :: Assignment Term
for = makeTerm <$> symbol For <*> children (Statement.ForEach <$> (makeTerm <$> location <*> manyTermsTill expression (symbol In)) <*> inClause <*> expressions)
  where inClause = symbol In *> children expression

case' :: Assignment Term
case' = makeTerm <$> symbol Case <*> children (Statement.Match <$> (symbol When *> emptyTerm <|> expression) <*> whens)
  where
    whens = makeTerm <$> location <*> many (when' <|> else' <|> expression)
    when' = makeTerm <$> symbol When <*> children (Statement.Pattern <$> (makeTerm <$> location <*> some pattern') <*> whens)
    pattern' = postContextualize comment (symbol Pattern *> children ((symbol SplatArgument *> children expression) <|> expression))
    else' = postContextualize comment (symbol Else *> children expressions)

subscript :: Assignment Term
subscript = makeTerm <$> symbol ElementReference <*> children (Expression.Subscript <$> expression <*> many expression)

pair :: Assignment Term
pair =   makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> (expression <|> emptyTerm))

args :: Assignment [Term]
args = (symbol ArgumentList <|> symbol ArgumentListWithParens) *> children (many expression) <|> many expression

methodCall :: Assignment Term
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

methodSelector :: Assignment Term
methodSelector = makeTerm <$> symbols <*> (Syntax.Identifier <$> (name <$> source))
  where
    symbols = symbol Identifier
          <|> symbol Constant
          <|> symbol Operator
          <|> symbol Setter
          <|> symbol Super -- TODO(@charliesome): super calls are *not* method calls and need to be assigned into their own syntax terms

call :: Assignment Term
call = makeTerm <$> symbol Call <*> children (
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> (Just <$> methodSelector) <*> pure [] <*> pure Nothing) <|>
    (Ruby.Syntax.Send <$> (Just <$> term expression) <*> pure Nothing <*> args <*> pure Nothing))

rescue :: Assignment Term
rescue =  rescue'
      <|> makeTerm <$> symbol RescueModifier <*> children (Statement.Try <$> expression <*> many (makeTerm <$> location <*> (Statement.Catch <$> expression <*> emptyTerm)))
      <|> makeTerm <$> symbol Ensure <*> children (Statement.Finally <$> expressions)
      <|> makeTerm <$> symbol Else <*> children (Statement.Else <$> emptyTerm <*> expressions)
  where
    rescue' = makeTerm <$> symbol Rescue <*> children (Statement.Catch <$> exceptions <*> (rescue' <|> expressions))
    exceptions = makeTerm <$> location <*> many ex
    ex =  makeTerm <$> symbol Exceptions <*> children (many expression)
      <|> makeTerm <$> symbol ExceptionVariable <*> children (many expression)

begin :: Assignment Term
begin = makeTerm <$> symbol Begin <*> children (Statement.Try <$> expressions <*> many rescue)

assignment' :: Assignment Term
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

identWithLocals :: Assignment (Record Location, Text, [Text])
identWithLocals = do
  loc <- symbol Identifier
  -- source advances, so it's important we call getLocals first
  locals <- getLocals
  ident <- source
  pure (loc, ident, locals)

lhsIdent :: Assignment Term
lhsIdent = do
  (loc, ident, locals) <- identWithLocals
  putLocals (ident : locals)
  pure $ makeTerm loc (Syntax.Identifier (name ident))

unary :: Assignment Term
unary = symbol Unary >>= \ location ->
      makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonBang *> expression )
  <|> makeTerm location . Expression.Not <$> children ( symbol AnonNot *> expression )
  <|> makeTerm location <$> children (Expression.Call [] <$> (makeTerm <$> symbol AnonDefinedQuestion <*> (Syntax.Identifier . name <$> source)) <*> some expression <*> emptyTerm)
  <|> makeTerm location . Expression.Negate <$> children ( symbol AnonMinus' *> expression )
  <|> children ( symbol AnonPlus *> expression )

-- TODO: Distinguish `===` from `==` ?
binary :: Assignment Term
binary = makeTerm' <$> symbol Binary <*> children (infixTerm expression expression
  [ (inject .) . Expression.Plus              <$ symbol AnonPlus
  , (inject .) . Expression.Minus             <$ symbol AnonMinus'
  , (inject .) . Expression.Times             <$ symbol AnonStar'
  , (inject .) . Expression.Power             <$ symbol AnonStarStar
  , (inject .) . Expression.DividedBy         <$ symbol AnonSlash
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

conditional :: Assignment Term
conditional = makeTerm <$> symbol Conditional <*> children (Statement.If <$> expression <*> expression <*> expression)

emptyStatement :: Assignment Term
emptyStatement = makeTerm <$> symbol EmptyStatement <*> (Syntax.Empty <$ rawSource <|> pure Syntax.Empty)


-- Helpers

invert :: Assignment Term -> Assignment Term
invert term = makeTerm <$> location <*> fmap Expression.Not term

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
term :: Assignment Term -> Assignment Term
term term = contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> heredocEnd) <*> emptyTerm)
  where heredocEnd = makeTerm <$> symbol HeredocEnd <*> (Literal.TextElement <$> source)

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment Term -> Assignment b -> Assignment [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment Term
          -> Assignment Term
          -> [Assignment (Term -> Term -> Sum Syntax Term)]
          -> Assignment (Sum Syntax Term)
infixTerm = infixContext comment

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
