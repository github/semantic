{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.MiniRuby.Assignment
(
-- Small version of Ruby to enable internal framework development.
  miniAssignment
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

-- | Small version of Ruby syntax for testing the code rewriting pipeline.
type MiniSyntax =
  '[ Comment.Comment
   , Declaration.Function
   , Declaration.Method
   , Expression.Minus
   , Expression.Plus
   , Expression.Times
   , Ruby.Syntax.Send
   , Statement.Statements
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Literal.Integer
   , []
   ]

type MiniTerm = Term.Term (Sum MiniSyntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar

miniAssignment :: Assignment MiniTerm
miniAssignment = handleError $ makeTerm <$> symbol Program <*> children (Statement.Statements <$> many expression) <|> parseError

expression :: Assignment MiniTerm
expression = term . handleError $
  choice [ binary
         , identifier
         , number
         , method
         , methodCall
         , parenthesizedExpressions ]

-- NOTE: Important that we don't flatten out the Imperative for single item lists
expressions :: Assignment MiniTerm
expressions = makeTerm <$> location <*> many expression

parenthesizedExpressions :: Assignment MiniTerm
parenthesizedExpressions = makeTerm'' <$> symbol ParenthesizedStatements <*> children (many expression)

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
  , (inject .) . Expression.Minus             <$ symbol AnonMinus'
  , (inject .) . Expression.Times             <$ symbol AnonStar'
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

identWithLocals :: Assignment (Record Location, Text, [Text])
identWithLocals = do
  loc <- symbol Identifier
  -- source advances, so it's important we call getLocals first
  locals <- getLocals
  ident <- source
  pure (loc, ident, locals)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
