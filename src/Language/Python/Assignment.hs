{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, TypeFamilies, TypeOperators #-}
module Language.Python.Assignment
( assignment
, Python.Syntax
, Grammar
, Python.Term(..)
) where

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (name)
import qualified Data.List.NonEmpty as NonEmpty
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
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import           Language.Python.Syntax as Python.Syntax
import           Language.Python.Term as Python
import           Prologue
import           TreeSitter.Python as Grammar

type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment (Term Loc)
assignment = handleError $ makeTerm <$> symbol Module <*> children (Statement.Statements <$> manyTerm expression) <|> parseError

expression :: Assignment (Term Loc)
expression = handleError (choice expressionChoices)

expressionChoices :: [Assignment (Term Loc)]
expressionChoices =
  -- Long-term, can we de/serialize assignments and avoid paying the cost of construction altogether?
  [ argumentList
  , assertStatement
  , assignment'
  , await
  , binaryOperator
  , block
  , boolean
  , booleanOperator
  , breakStatement
  , call
  , classDefinition
  , comparisonOperator
  , comprehension
  , concatenatedString
  , conditionalExpression
  , continueStatement
  , decoratedDefinition
  , deleteStatement
  , dictionary
  , dictionarySplat
  , ellipsis
  , exceptClause
  , execStatement
  , expressionList
  , expressionStatement
  , finallyClause
  , float
  , forInClause
  , forStatement
  , functionDefinition
  , globalStatement
  , identifier
  , ifClause
  , ifStatement
  , import'
  , integer
  , keywordArgument
  , list'
  , listSplat
  , memberAccess
  , none
  , nonlocalStatement
  , notOperator
  , pair
  , parameter
  , parenthesizedExpression
  , parseError
  , passStatement
  , printStatement
  , raiseStatement
  , returnStatement
  , set
  , slice
  , string
  , subscript
  , tryStatement
  , tuple
  , type'
  , unaryOperator
  , variables
  , whileStatement
  , withStatement
  , yield
  ]

expressions :: Assignment (Term Loc)
expressions = makeTerm'' <$> location <*> manyTerm expression

block :: Assignment (Term Loc)
block = symbol Block *> children (makeTerm'' <$> location <*> manyTerm expression)

block' :: Assignment (Term Loc)
block' = symbol Block *> children (makeTerm <$> location <*> manyTerm expression)

expressionStatement :: Assignment (Term Loc)
expressionStatement = makeTerm'' <$> symbol ExpressionStatement <*> children (someTerm expression)

expressionList :: Assignment (Term Loc)
expressionList = makeTerm'' <$> symbol ExpressionList <*> children (someTerm expression)

listSplat :: Assignment (Term Loc)
listSplat = makeTerm <$> symbol ListSplat <*> (Syntax.Identifier . name <$> source)

dictionarySplat :: Assignment (Term Loc)
dictionarySplat = makeTerm <$> symbol DictionarySplat <*> (Syntax.Identifier . name <$> source)

keywordArgument :: Assignment (Term Loc)
keywordArgument = makeTerm <$> symbol KeywordArgument <*> children (Statement.Assignment [] <$> term expression <*> term expression)

parenthesizedExpression :: Assignment (Term Loc)
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

parameter :: Assignment (Term Loc)
parameter =  makeTerm <$> symbol DefaultParameter <*> children (Statement.Assignment [] <$> term expression <*> term expression)
         <|> makeTerm <$> symbol TypedParameter <*> children (Type.Annotation <$> term expression <*> term type')
         <|> makeAnnotation <$> symbol TypedDefaultParameter <*> children ((,,) <$> term expression <*> term expression <*> term expression)
  where
    makeAnnotation loc (identifier', type', value') = makeTerm loc (Type.Annotation (makeAssignment loc identifier' value') type')
    makeAssignment loc identifier' value' = makeTerm loc (Statement.Assignment [] identifier' value')

decoratedDefinition :: Assignment (Term Loc)
decoratedDefinition = symbol DecoratedDefinition *> children (term decorator)
  where
    decorator = makeTerm <$> symbol Decorator <*> (children (Declaration.Decorator <$> term expression <*> manyTerm expression) <*> term (decorator <|> functionDefinition <|> classDefinition))

argumentList :: Assignment (Term Loc)
argumentList = symbol ArgumentList *> children expressions

withStatement :: Assignment (Term Loc)
withStatement = symbol WithStatement *> children (flip (foldr make) <$> some withItem <*> term block')
  where
    make (val, name) = makeTerm1 . Statement.Let name val
    withItem = symbol WithItem *> children ((,) <$> term expression <*> term (expression <|> emptyTerm))

forStatement :: Assignment (Term Loc)
forStatement = symbol ForStatement >>= \ loc -> children (make loc <$> (symbol Variables *> children expressions) <*> term expressionList <*> term block' <*> optional (symbol ElseClause *> children expressions))
  where
    make loc binding subject body forElseClause = case forElseClause of
      Nothing -> makeTerm loc (Statement.ForEach binding subject body)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.ForEach binding subject body) a)

whileStatement :: Assignment (Term Loc)
whileStatement = symbol WhileStatement >>= \ loc -> children (make loc <$> term expression <*> term block <*> optional (symbol ElseClause *> children expressions))
  where
    make loc whileCondition whileBody whileElseClause = case whileElseClause of
      Nothing -> makeTerm loc (Statement.While whileCondition whileBody)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.While whileCondition whileBody) a)

tryStatement :: Assignment (Term Loc)
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term block <*> manyTerm (expression <|> elseClause))
  where elseClause = makeTerm <$> symbol ElseClause <*> children (Statement.Else <$> emptyTerm <*> term block)

exceptClause :: Assignment (Term Loc)
exceptClause = makeTerm <$> symbol ExceptClause <*> children
  (Statement.Catch <$> term ((makeTerm <$> location <*> (uncurry (flip Statement.Let) <$> ((,) <$> term expression <* symbol AnonAs <*> term expression) <*> emptyTerm))
                      <|> expressions)
                   <*> expressions)

functionParam :: Assignment (Term Loc)
functionParam = (makeParameter <$> location <*> identifier)
  <|> tuple
  <|> parameter
  <|> listSplat
  <|> dictionarySplat
  where makeParameter loc term = makeTerm loc (Declaration.RequiredParameter term)

functionDefinition :: Assignment (Term Loc)
functionDefinition =
      makeFunctionDeclaration <$> symbol FunctionDefinition <*> children ((,,,) <$> term expression <* symbol Parameters <*> children (manyTerm functionParam) <*> optional (symbol Type *> children (term expression)) <*> term block')
  <|> makeFunctionDeclaration <$> (symbol Lambda' <|> symbol Lambda) <*> children ((,,,) <$ token AnonLambda <*> emptyTerm <*> (symbol LambdaParameters *> children (manyTerm expression) <|> pure []) <*> optional (symbol Type *> children (term expression)) <*> expressions')
  where
    expressions' = makeTerm <$> location <*> manyTerm expression
    makeFunctionDeclaration loc (functionName', functionParameters, ty, functionBody)
      = let fn = makeTerm loc (Declaration.Function [] functionName' functionParameters functionBody)
        in maybe fn (makeTerm loc . Type.Annotation fn) ty

classDefinition :: Assignment (Term Loc)
classDefinition = makeTerm <$> symbol ClassDefinition <*> children (Declaration.Class [] <$> term expression <*> argumentList <*> term block')
  where
    argumentList = symbol ArgumentList *> children (manyTerm expression)
                    <|> pure []

type' :: Assignment (Term Loc)
type' = symbol Type *> children (term expression)

finallyClause :: Assignment (Term Loc)
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> expressions)

ellipsis :: Assignment (Term Loc)
ellipsis = makeTerm <$> token Grammar.Ellipsis <*> pure Python.Syntax.Ellipsis

comparisonOperator :: Assignment (Term Loc)
comparisonOperator = symbol ComparisonOperator *> children (expression `chainl1Term` choice
  [ (makeTerm1 .) . Expression.LessThan         <$ token AnonLAngle
  , (makeTerm1 .) . Expression.LessThanEqual    <$ token AnonLAngleEqual
  , (makeTerm1 .) . Expression.GreaterThan      <$ token AnonRAngle
  , (makeTerm1 .) . Expression.GreaterThanEqual <$ token AnonRAngleEqual
  , (makeTerm1 .) . Expression.Equal            <$ token AnonEqualEqual
  , (makeTerm1 .) . invert Expression.Equal     <$ token AnonBangEqual
  , (makeTerm1 .) . invert Expression.Equal     <$ token AnonLAngleRAngle
  , (makeTerm1 .) . invert Expression.Member    <$ token AnonNot
  , (makeTerm1 .) . Expression.Member           <$ token AnonIn
  , token AnonIs *> ((makeTerm1 .) . invert Expression.Equal <$ token AnonNot <|> pure ((makeTerm1 .) . Expression.Equal))
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

notOperator :: Assignment (Term Loc)
notOperator = makeTerm <$> symbol NotOperator <*> children (Expression.Not <$> term expression)

tuple :: Assignment (Term Loc)
tuple = makeTerm <$> symbol Tuple <*> children (Literal.Tuple <$> manyTerm expression)

unaryOperator :: Assignment (Term Loc)
unaryOperator = symbol UnaryOperator >>= \ location -> arithmetic location <|> bitwise location <|> children ( symbol AnonPlus *> term expression )
  where
    arithmetic location = makeTerm location . Expression.Negate <$> children ( symbol AnonMinus *> term expression )
    bitwise location    = makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> term expression )

binaryOperator :: Assignment (Term Loc)
binaryOperator = makeTerm' <$> symbol BinaryOperator <*> children (infixTerm expression (term expression)
  [ (inject .) . Expression.Plus      <$ symbol AnonPlus
  , (inject .) . Expression.Minus     <$ symbol AnonMinus
  , (inject .) . Expression.Times     <$ symbol AnonStar
  , (inject .) . Expression.Times     <$ symbol AnonAt -- Matrix multiplication, TODO: May not want to assign to Expression.Times.
  , (inject .) . Expression.DividedBy <$ symbol AnonSlash
  , (inject .) . Expression.FloorDivision <$ symbol AnonSlashSlash
  , (inject .) . Expression.Modulo    <$ symbol AnonPercent
  , (inject .) . Expression.Power     <$ symbol AnonStarStar
  , (inject .) . Expression.BOr       <$ symbol AnonPipe
  , (inject .) . Expression.BAnd      <$ symbol AnonAmpersand
  , (inject .) . Expression.BXOr      <$ symbol AnonCaret
  , (inject .) . Expression.LShift    <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift    <$ symbol AnonRAngleRAngle
  ])

booleanOperator :: Assignment (Term Loc)
booleanOperator = makeTerm' <$> symbol BooleanOperator <*> children (infixTerm expression (term expression)
  [ (inject .) . Expression.And <$ symbol AnonAnd
  , (inject .) . Expression.Or  <$ symbol AnonOr
  ])

assignment' :: Assignment (Term Loc)
assignment' =  makeAssignment <$> symbol Assignment <*> children ((,,) <$> term expressionList <*> optional (symbol Type *> children (term expression)) <*> term rvalue)
           <|> makeTerm' <$> symbol AugmentedAssignment <*> children (infixTerm expressionList (term rvalue)
                  [ assign Expression.Plus      <$ symbol AnonPlusEqual
                  , assign Expression.Minus     <$ symbol AnonMinusEqual
                  , assign Expression.Times     <$ symbol AnonStarEqual
                  , assign Expression.Times     <$ symbol AnonAtEqual -- Matrix multiplication assignment. TODO: May not want to assign to Expression.Times.
                  , assign Expression.Power     <$ symbol AnonStarStarEqual
                  , assign Expression.DividedBy <$ symbol AnonSlashEqual
                  , assign Expression.DividedBy <$ symbol AnonSlashSlashEqual
                  , assign Expression.BOr       <$ symbol AnonPipeEqual
                  , assign Expression.BAnd      <$ symbol AnonAmpersandEqual
                  , assign Expression.Modulo    <$ symbol AnonPercentEqual
                  , assign Expression.RShift    <$ symbol AnonRAngleRAngleEqual
                  , assign Expression.LShift    <$ symbol AnonLAngleLAngleEqual
                  , assign Expression.BXOr      <$ symbol AnonCaretEqual
                  ])
  where rvalue = expressionList <|> assignment' <|> yield <|> emptyTerm
        makeAssignment loc (lhs, maybeType, rhs) = makeTerm loc (Statement.Assignment (maybeToList maybeType) lhs rhs)
        assign :: (f :< Python.Syntax) => (Term Loc -> Term Loc -> f (Term Loc)) -> Term Loc -> Term Loc -> Sum Python.Syntax (Term Loc)
        assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))

yield :: Assignment (Term Loc)
yield = makeTerm <$> symbol Yield <*> (Statement.Yield <$> children (term ( expression <|> emptyTerm )))

identifier :: Assignment (Term Loc)
identifier = makeTerm <$> (symbol Identifier <|> symbol DottedName) <*> (Syntax.Identifier . name <$> source)

set :: Assignment (Term Loc)
set = makeTerm <$> symbol Set <*> children (Literal.Set <$> manyTerm expression)

dictionary :: Assignment (Term Loc)
dictionary = makeTerm <$> symbol Dictionary <*> children (Literal.Hash <$> manyTerm expression)

pair :: Assignment (Term Loc)
pair = makeTerm' <$> symbol Pair <*> children (infixTerm expression (term expression) [ (inject .) . Literal.KeyValue <$ symbol AnonColon ])

list' :: Assignment (Term Loc)
list' = makeTerm <$> symbol List <*> children (Literal.Array <$> manyTerm expression)

string :: Assignment (Term Loc)
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

concatenatedString :: Assignment (Term Loc)
concatenatedString = makeTerm <$> symbol ConcatenatedString <*> children (manyTerm string)

float :: Assignment (Term Loc)
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: Assignment (Term Loc)
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: Assignment (Term Loc)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

import' :: Assignment (Term Loc)
import' =   makeTerm'' <$> symbol ImportStatement <*> children (manyTerm (aliasedImport <|> plainImport))
        <|> makeTerm <$> symbol ImportFromStatement <*> children (Python.Syntax.Import <$> importPath <*> (wildcard <|> some (aliasImportSymbol <|> importSymbol)))
        <|> makeTerm <$> symbol FutureImportStatement <*> children (Python.Syntax.FutureImport <$> some (aliasImportSymbol <|> importSymbol))
  where
    -- `import a as b`
    aliasedImport = makeTerm <$> symbol AliasedImport <*> children (Python.Syntax.QualifiedAliasedImport  <$> importPath <*> expression)
    -- `import a`
    plainImport = makeTerm <$> symbol DottedName <*> children (Python.Syntax.QualifiedImport  <$> NonEmpty.some1 identifier)
    -- `from a import foo `
    importSymbol = makeNameAliasPair <$> (symbol Identifier <|> symbol DottedName) <*> (mkIdentifier <$> location <*> source)
    -- `from a import foo as bar`
    aliasImportSymbol = makeTerm <$> symbol AliasedImport <*> children (Python.Syntax.Alias <$> identifier <*> identifier)
    -- `from a import *`
    wildcard = symbol WildcardImport *> (name <$> source) $> []

    importPath = importDottedName <|> importRelative
    importDottedName = symbol DottedName *> children (qualifiedName <$> NonEmpty.some1 identifierSource)
    importRelative = symbol RelativeImport *> children (relativeQualifiedName <$> importPrefix <*> ((symbol DottedName *> children (many identifierSource)) <|> pure []))
    importPrefix = symbol ImportPrefix *> source
    identifierSource = symbol Identifier *> source

    makeNameAliasPair location alias = makeTerm location (Python.Syntax.Alias alias alias)
    mkIdentifier location source = makeTerm location (Syntax.Identifier (name source))

assertStatement :: Assignment (Term Loc)
assertStatement = makeTerm <$> symbol AssertStatement <*> children (Expression.Call [] <$> (makeTerm <$> symbol AnonAssert <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

printStatement :: Assignment (Term Loc)
printStatement = do
  location <- symbol PrintStatement
  children $ do
    print <- term printKeyword
    term (redirectCallTerm location print <|> printCallTerm location print)
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier . name <$> source)
    redirectCallTerm location identifier = makeTerm location <$ symbol Chevron <*> (flip Python.Syntax.Redirect <$> children (term expression) <*> term (printCallTerm location identifier))
    printCallTerm location identifier = makeTerm location <$> (Expression.Call [] identifier <$> manyTerm expression <*> emptyTerm)

nonlocalStatement :: Assignment (Term Loc)
nonlocalStatement = makeTerm <$> symbol NonlocalStatement <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonNonlocal <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

globalStatement :: Assignment (Term Loc)
globalStatement = makeTerm <$> symbol GlobalStatement <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonGlobal <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

await :: Assignment (Term Loc)
await = makeTerm <$> symbol Await <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonAwait <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

returnStatement :: Assignment (Term Loc)
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> term (expressionList <|> emptyTerm))

deleteStatement :: Assignment (Term Loc)
deleteStatement = makeTerm <$> symbol DeleteStatement <*> children (Expression.Call [] <$> term deleteIdentifier <* symbol ExpressionList <*> children (manyTerm expression) <*> emptyTerm)
  where deleteIdentifier = makeTerm <$> symbol AnonDel <*> (Syntax.Identifier . name <$> source)

raiseStatement :: Assignment (Term Loc)
raiseStatement = makeTerm <$> symbol RaiseStatement <*> children (Statement.Throw <$> expressions)

ifStatement :: Assignment (Term Loc)
ifStatement = makeTerm <$> symbol IfStatement <*> children if'
  where
    if' = Statement.If <$> term expression <*> thenClause <*> (elseClause <|> emptyTerm)
    thenClause = makeTerm'' <$> location <*> manyTermsTill expression (void (symbol ElseClause) <|> void (symbol ElifClause) <|> eof)
    elseClause = makeTerm'' <$> location <*> many (comment <|> elif <|> else')
    elif = makeTerm <$> symbol ElifClause <*> children if'
    else' = symbol ElseClause *> children expressions

execStatement :: Assignment (Term Loc)
execStatement = makeTerm <$> symbol ExecStatement <*> children (Expression.Call [] <$> term (makeTerm <$> location <*> (Syntax.Identifier . name <$> source)) <*> manyTerm (string <|> expression) <*> emptyTerm)

passStatement :: Assignment (Term Loc)
passStatement = makeTerm <$> symbol PassStatement <*> (Statement.NoOp <$> emptyTerm <* advance)

breakStatement :: Assignment (Term Loc)
breakStatement = makeTerm <$> symbol BreakStatement <*> (Statement.Break <$> emptyTerm <* advance)

continueStatement :: Assignment (Term Loc)
continueStatement = makeTerm <$> symbol ContinueStatement <*> (Statement.Continue <$> emptyTerm <* advance)

memberAccess :: Assignment (Term Loc)
memberAccess = makeTerm <$> symbol Attribute <*> children (Expression.MemberAccess <$> term expression <*> identifier)

subscript :: Assignment (Term Loc)
subscript = makeTerm <$> symbol Subscript <*> children (Expression.Subscript <$> term expression <*> manyTerm expression)

slice :: Assignment (Term Loc)
slice = makeTerm <$> symbol Slice <*> children
  (Expression.Enumeration <$> ((emptyTerm <* token AnonColon) <|> (term expression <* token AnonColon))
                          <*> ((emptyTerm <* token AnonColon) <|> (term expression <* token AnonColon) <|> (term expression <|> emptyTerm))
                          <*> (term expression <|> emptyTerm))

call :: Assignment (Term Loc)
call = makeTerm <$> symbol Call <*> children (Expression.Call [] <$> term (identifier <|> expression) <*> (symbol ArgumentList *> children (manyTerm expression) <|> someTerm comprehension) <*> emptyTerm)

boolean :: Assignment (Term Loc)
boolean =  makeTerm <$> token Grammar.True <*> pure Literal.true
       <|> makeTerm <$> token Grammar.False <*> pure Literal.false

none :: Assignment (Term Loc)
none = makeTerm <$> symbol None <*> (Literal.Null <$ rawSource)

comprehension :: Assignment (Term Loc)
comprehension =  makeTerm <$> symbol ListComprehension       <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol GeneratorExpression     <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol SetComprehension        <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol DictionaryComprehension <*> children (Declaration.Comprehension <$> term expression <*> expressions)

forInClause :: Assignment (Term Loc)
forInClause = symbol ForInClause *> children expressions

variables :: Assignment (Term Loc)
variables = symbol Variables *> children expressions

ifClause :: Assignment (Term Loc)
ifClause = symbol IfClause *> children expressions

conditionalExpression :: Assignment (Term Loc)
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (flip Statement.If <$> term expression <*> term expression <*> expressions)


-- Helpers

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment (Term Loc) -> Assignment (Term Loc)
term term = contextualize comment (postContextualize comment term)

term' :: Assignment (Term Loc) -> Assignment (Term Loc)
term' term = contextualize comment' (postContextualize comment' term)
  where comment' = choice [ comment, symbol AnonLambda *> empty ]

-- | Match a left-associated infix chain of terms, optionally followed by comments. Like 'chainl1' but assigning comment nodes automatically.
chainl1Term :: Assignment (Term Loc) -> Assignment (Term Loc -> Term Loc -> Term Loc) -> Assignment (Term Loc)
chainl1Term expr op = term' expr `chainl1` op

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment (Term Loc) -> Assignment b -> Assignment [Term Loc]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment (Term Loc)
          -> Assignment (Term Loc)
          -> [Assignment (Term Loc -> Term Loc -> Sum Python.Syntax (Term Loc))]
          -> Assignment (Sum Python.Syntax (Term Loc))
infixTerm = infixContext comment
