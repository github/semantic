{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Python.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (Name, name)
import qualified Data.Diff as Diff
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
import qualified Data.Term as Term
import qualified Data.Text as T
import           Language.Python.Grammar as Grammar
import           Language.Python.Syntax as Python.Syntax
import           Prologue
import           Proto3.Suite (Named (..), Named1 (..))


-- | The type of Python syntax.
type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.Comprehension
   , Declaration.Decorator
   , Declaration.Function
   , Expression.Plus
   , Expression.Minus
   , Expression.Times
   , Expression.DividedBy
   , Expression.Modulo
   , Expression.Power
   , Expression.Negate
   , Expression.FloorDivision
   , Expression.And
   , Expression.Not
   , Expression.Or
   , Expression.XOr
   , Expression.BAnd
   , Expression.BOr
   , Expression.BXOr
   , Expression.LShift
   , Expression.RShift
   , Expression.Complement
   , Expression.Call
   , Expression.LessThan
   , Expression.LessThanEqual
   , Expression.GreaterThan
   , Expression.GreaterThanEqual
   , Expression.Equal
   , Expression.StrictEqual
   , Expression.Comparison
   , Expression.Enumeration
   , Expression.ScopeResolution
   , Expression.MemberAccess
   , Expression.Subscript
   , Expression.Member
   , Literal.Array
   , Literal.Boolean
   , Literal.Float
   , Literal.Hash
   , Literal.Integer
   , Literal.KeyValue
   , Literal.Null
   , Literal.Set
   , Literal.String
   , Literal.TextElement
   , Literal.Tuple
   , Python.Syntax.Ellipsis
   , Python.Syntax.FutureImport
   , Python.Syntax.Import
   , Python.Syntax.QualifiedImport
   , Python.Syntax.QualifiedAliasedImport
   , Python.Syntax.Redirect
   , Statement.Assignment
   , Statement.Break
   , Statement.Catch
   , Statement.Continue
   , Statement.Else
   , Statement.Finally
   , Statement.ForEach
   , Statement.If
   , Statement.Let
   , Statement.NoOp
   , Statement.Return
   , Statement.Statements
   , Statement.Throw
   , Statement.Try
   , Statement.While
   , Statement.Yield
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Type.Annotation
   , []
   ]

type Term = Term.Term (Sum Syntax) Location
type Assignment = Assignment.Assignment [] Grammar

instance Named1 (Sum Syntax) where
  nameOf1 _ = "PythonSyntax"

instance Named (Term.Term (Sum Syntax) ()) where
  nameOf _ = "PythonTerm"

instance Named (Diff.Diff (Sum Syntax) () ()) where
  nameOf _ = "PythonDiff"

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment Term
assignment = handleError $ makeTerm <$> symbol Module <*> children (Statement.Statements <$> manyTerm expression) <|> parseError

expression :: Assignment Term
expression = handleError (choice expressionChoices)

expressionChoices :: [Assignment Term]
expressionChoices =
  -- Long-term, can we de/serialize assignments and avoid paying the cost of construction altogether?
  [ argumentList
  , assertStatement
  , assignment'
  , await
  , binaryOperator
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

expressions :: Assignment Term
expressions = makeTerm'' <$> location <*> manyTerm expression

expressionStatement :: Assignment Term
expressionStatement = makeTerm'' <$> symbol ExpressionStatement <*> children (someTerm expression)

expressionList :: Assignment Term
expressionList = makeTerm'' <$> symbol ExpressionList <*> children (someTerm expression)

listSplat :: Assignment Term
listSplat = makeTerm <$> symbol ListSplat <*> (Syntax.Identifier . name <$> source)

dictionarySplat :: Assignment Term
dictionarySplat = makeTerm <$> symbol DictionarySplat <*> (Syntax.Identifier . name <$> source)

keywordArgument :: Assignment Term
keywordArgument = makeTerm <$> symbol KeywordArgument <*> children (Statement.Assignment [] <$> term expression <*> term expression)

parenthesizedExpression :: Assignment Term
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

parameter :: Assignment Term
parameter =  makeTerm <$> symbol DefaultParameter <*> children (Statement.Assignment [] <$> term expression <*> term expression)
         <|> makeTerm <$> symbol TypedParameter <*> children (Type.Annotation <$> term expression <*> term type')
         <|> makeAnnotation <$> symbol TypedDefaultParameter <*> children ((,,) <$> term expression <*> term expression <*> term expression)
  where
    makeAnnotation loc (identifier', type', value') = makeTerm loc (Type.Annotation (makeAssignment loc identifier' value') type')
    makeAssignment loc identifier' value' = makeTerm loc (Statement.Assignment [] identifier' value')

decoratedDefinition :: Assignment Term
decoratedDefinition = symbol DecoratedDefinition *> children (term decorator)
  where
    decorator = makeTerm <$> symbol Decorator <*> (children (Declaration.Decorator <$> term expression <*> manyTerm expression) <*> term (decorator <|> functionDefinition <|> classDefinition))

argumentList :: Assignment Term
argumentList = symbol ArgumentList *> children expressions

withStatement :: Assignment Term
withStatement = mk <$> symbol WithStatement <*> children (someTerm with)
  where
    mk _ [child] = child
    mk l children = makeTerm l children
    with = makeTerm <$> location <*> (withItem <*> term (makeTerm <$> location <*> manyTermsTill expression (void (symbol WithItem) <|> eof)))
    withItem = symbol WithItem *> children (flip Statement.Let <$> term expression <*> term (expression <|> emptyTerm))
            <|> flip Statement.Let <$> term expression <*> emptyTerm

forStatement :: Assignment Term
forStatement = symbol ForStatement >>= \ loc -> children (make loc <$> (symbol Variables *> children expressions) <*> term expressionList <*> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ElseClause) <|> eof)) <*> optional (symbol ElseClause *> children expressions))
  where
    make loc binding subject body forElseClause = case forElseClause of
      Nothing -> makeTerm loc (Statement.ForEach binding subject body)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.ForEach binding subject body) a)

whileStatement :: Assignment Term
whileStatement = symbol WhileStatement >>= \ loc -> children (make loc <$> term expression <*> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ElseClause) <|> eof)) <*> optional (symbol ElseClause *> children expressions))
  where
    make loc whileCondition whileBody whileElseClause = case whileElseClause of
      Nothing -> makeTerm loc (Statement.While whileCondition whileBody)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.While whileCondition whileBody) a)

tryStatement :: Assignment Term
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> term expression <*> manyTerm (expression <|> elseClause))
  where elseClause = makeTerm <$> symbol ElseClause <*> children (Statement.Else <$> emptyTerm <*> expressions)

exceptClause :: Assignment Term
exceptClause = makeTerm <$> symbol ExceptClause <*> children
  (Statement.Catch <$> term ((makeTerm <$> location <*> (uncurry (flip Statement.Let) <$> ((,) <$> term expression <* symbol AnonAs <*> term expression) <*> emptyTerm))
                      <|> expressions)
                   <*> expressions)

functionDefinition :: Assignment Term
functionDefinition =
      makeFunctionDeclaration <$> symbol FunctionDefinition <*> children ((,,,) <$> term expression <* symbol Parameters <*> children (manyTerm expression) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  <|> makeFunctionDeclaration <$> (symbol Lambda' <|> symbol Lambda) <*> children ((,,,) <$ token AnonLambda <*> emptyTerm <*> (symbol LambdaParameters *> children (manyTerm expression) <|> pure []) <*> optional (symbol Type *> children (term expression)) <*> expressions)
  where
    makeFunctionDeclaration loc (functionName', functionParameters, ty, functionBody)
      = let fn = makeTerm loc (Declaration.Function [] functionName' functionParameters functionBody)
        in maybe fn (makeTerm loc . Type.Annotation fn) ty

classDefinition :: Assignment Term
classDefinition = makeTerm <$> symbol ClassDefinition <*> children (Declaration.Class [] <$> term expression <*> argumentList <*> expressions)
  where argumentList = symbol ArgumentList *> children (manyTerm expression)
                    <|> pure []

type' :: Assignment Term
type' = symbol Type *> children (term expression)

finallyClause :: Assignment Term
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> expressions)

ellipsis :: Assignment Term
ellipsis = makeTerm <$> token Grammar.Ellipsis <*> pure Python.Syntax.Ellipsis

comparisonOperator :: Assignment Term
comparisonOperator = symbol ComparisonOperator *> children (expression `chainl1Term` choice
  [ (makeTerm1 .) . Expression.LessThan         <$ symbol AnonLAngle
  , (makeTerm1 .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (makeTerm1 .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (makeTerm1 .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (makeTerm1 .) . Expression.Equal            <$ symbol AnonEqualEqual
  , (makeTerm1 .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (makeTerm1 .) . invert Expression.Equal     <$ symbol AnonLAngleRAngle
  , (makeTerm1 .) . invert Expression.Member    <$ symbol AnonNot
  , (makeTerm1 .) . Expression.Member           <$ symbol AnonIn
  , token AnonIs *> ((makeTerm1 .) . invert Expression.Equal <$ symbol AnonNot <|> pure ((makeTerm1 .) . Expression.Equal))
  ])
  where invert cons a b = Expression.Not (makeTerm1 (cons a b))

notOperator :: Assignment Term
notOperator = makeTerm <$> symbol NotOperator <*> children (Expression.Not <$> term expression)

tuple :: Assignment Term
tuple = makeTerm <$> symbol Tuple <*> children (Literal.Tuple <$> manyTerm expression)

unaryOperator :: Assignment Term
unaryOperator = symbol UnaryOperator >>= \ location -> arithmetic location <|> bitwise location <|> children ( symbol AnonPlus *> term expression )
  where
    arithmetic location = makeTerm location . Expression.Negate <$> children ( symbol AnonMinus *> term expression )
    bitwise location    = makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> term expression )

binaryOperator :: Assignment Term
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

booleanOperator :: Assignment Term
booleanOperator = makeTerm' <$> symbol BooleanOperator <*> children (infixTerm expression (term expression)
  [ (inject .) . Expression.And <$ symbol AnonAnd
  , (inject .) . Expression.Or  <$ symbol AnonOr
  ])

assignment' :: Assignment Term
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
  where rvalue = expressionList <|> assignment' <|> yield
        makeAssignment loc (lhs, maybeType, rhs) = makeTerm loc (Statement.Assignment (maybeToList maybeType) lhs rhs)
        assign :: (f :< Syntax) => (Term -> Term -> f Term) -> Term -> Term -> Sum Syntax Term
        assign c l r = inject (Statement.Assignment [] l (makeTerm1 (c l r)))

yield :: Assignment Term
yield = makeTerm <$> symbol Yield <*> (Statement.Yield <$> children (term ( expression <|> emptyTerm )))

identifier :: Assignment Term
identifier = makeTerm <$> (symbol Identifier <|> symbol Identifier' <|> symbol DottedName) <*> (Syntax.Identifier . name <$> source)

identifier' :: Assignment Name
identifier' = (symbol Identifier <|> symbol Identifier' <|> symbol DottedName) *> (name <$> source)

set :: Assignment Term
set = makeTerm <$> symbol Set <*> children (Literal.Set <$> manyTerm expression)

dictionary :: Assignment Term
dictionary = makeTerm <$> symbol Dictionary <*> children (Literal.Hash <$> manyTerm expression)

pair :: Assignment Term
pair = makeTerm' <$> symbol Pair <*> children (infixTerm expression (term expression) [ (inject .) . Literal.KeyValue <$ symbol AnonColon ])

list' :: Assignment Term
list' = makeTerm <$> symbol List <*> children (Literal.Array <$> manyTerm expression)

string :: Assignment Term
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

concatenatedString :: Assignment Term
concatenatedString = makeTerm <$> symbol ConcatenatedString <*> children (manyTerm string)

float :: Assignment Term
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: Assignment Term
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: Assignment Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

import' :: Assignment Term
import' =   makeTerm'' <$> symbol ImportStatement <*> children (manyTerm (aliasedImport <|> plainImport))
        <|> makeTerm <$> symbol ImportFromStatement <*> children (Python.Syntax.Import <$> importPath <*> (wildcard <|> some (aliasImportSymbol <|> importSymbol)))
        <|> makeTerm <$> symbol FutureImportStatement <*> children (Python.Syntax.FutureImport <$> some (aliasImportSymbol <|> importSymbol))
  where
    -- `import a as b`
    aliasedImport = makeTerm <$> symbol AliasedImport <*> children (Python.Syntax.QualifiedAliasedImport  <$> importPath <*> expression)
    -- `import a`
    plainImport = makeTerm <$> symbol DottedName <*> children (Python.Syntax.QualifiedImport . NonEmpty.map T.unpack <$> NonEmpty.some1 identifierSource)
    -- `from a import foo `
    importSymbol = makeNameAliasPair <$> aliasIdentifier <*> pure Nothing
    -- `from a import foo as bar`
    aliasImportSymbol = symbol AliasedImport *> children (makeNameAliasPair <$> aliasIdentifier <*> (Just <$> aliasIdentifier))
    -- `from a import *`
    wildcard = symbol WildcardImport *> (name <$> source) $> []

    importPath = importDottedName <|> importRelative
    importDottedName = symbol DottedName *> children (qualifiedName <$> NonEmpty.some1 identifierSource)
    importRelative = symbol RelativeImport *> children (relativeQualifiedName <$> importPrefix <*> ((symbol DottedName *> children (many identifierSource)) <|> pure []))
    importPrefix = symbol ImportPrefix *> source
    identifierSource = (symbol Identifier <|> symbol Identifier') *> source

    aliasIdentifier = (symbol Identifier <|> symbol Identifier') *> (name <$> source) <|> symbol DottedName *> (name <$> source)
    makeNameAliasPair from (Just alias) = Python.Syntax.Alias from alias
    makeNameAliasPair from Nothing = Python.Syntax.Alias from from

assertStatement :: Assignment Term
assertStatement = makeTerm <$> symbol AssertStatement <*> children (Expression.Call [] <$> (makeTerm <$> symbol AnonAssert <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

printStatement :: Assignment Term
printStatement = do
  location <- symbol PrintStatement
  children $ do
    print <- term printKeyword
    term (redirectCallTerm location print <|> printCallTerm location print)
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier . name <$> source)
    redirectCallTerm location identifier = makeTerm location <$ symbol Chevron <*> (flip Python.Syntax.Redirect <$> children (term expression) <*> term (printCallTerm location identifier))
    printCallTerm location identifier = makeTerm location <$> (Expression.Call [] identifier <$> manyTerm expression <*> emptyTerm)

nonlocalStatement :: Assignment Term
nonlocalStatement = makeTerm <$> symbol NonlocalStatement <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonNonlocal <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

globalStatement :: Assignment Term
globalStatement = makeTerm <$> symbol GlobalStatement <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonGlobal <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

await :: Assignment Term
await = makeTerm <$> symbol Await <*> children (Expression.Call [] <$> term (makeTerm <$> symbol AnonAwait <*> (Syntax.Identifier . name <$> source)) <*> manyTerm expression <*> emptyTerm)

returnStatement :: Assignment Term
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> term (expressionList <|> emptyTerm))

deleteStatement :: Assignment Term
deleteStatement = makeTerm <$> symbol DeleteStatement <*> children (Expression.Call [] <$> term deleteIdentifier <* symbol ExpressionList <*> children (manyTerm expression) <*> emptyTerm)
  where deleteIdentifier = makeTerm <$> symbol AnonDel <*> (Syntax.Identifier . name <$> source)

raiseStatement :: Assignment Term
raiseStatement = makeTerm <$> symbol RaiseStatement <*> children (Statement.Throw <$> expressions)

ifStatement :: Assignment Term
ifStatement = makeTerm <$> symbol IfStatement <*> children if'
  where
    if' = Statement.If <$> term expression <*> thenClause <*> (elseClause <|> emptyTerm)
    thenClause = makeTerm <$> location <*> manyTermsTill expression (void (symbol ElseClause) <|> void (symbol ElifClause) <|> eof)
    elseClause = makeTerm <$> location <*> many (comment <|> elif <|> else')
    elif = makeTerm <$> symbol ElifClause <*> children if'
    else' = symbol ElseClause *> children expressions

execStatement :: Assignment Term
execStatement = makeTerm <$> symbol ExecStatement <*> children (Expression.Call [] <$> term (makeTerm <$> location <*> (Syntax.Identifier . name <$> source)) <*> manyTerm (string <|> expression) <*> emptyTerm)

passStatement :: Assignment Term
passStatement = makeTerm <$> symbol PassStatement <*> (Statement.NoOp <$> emptyTerm <* advance)

breakStatement :: Assignment Term
breakStatement = makeTerm <$> symbol BreakStatement <*> (Statement.Break <$> emptyTerm <* advance)

continueStatement :: Assignment Term
continueStatement = makeTerm <$> symbol ContinueStatement <*> (Statement.Continue <$> emptyTerm <* advance)

memberAccess :: Assignment Term
memberAccess = makeTerm <$> symbol Attribute <*> children (Expression.MemberAccess <$> expression <*> identifier')

subscript :: Assignment Term
subscript = makeTerm <$> symbol Subscript <*> children (Expression.Subscript <$> term expression <*> manyTerm expression)

slice :: Assignment Term
slice = makeTerm <$> symbol Slice <*> children
  (Expression.Enumeration <$> ((emptyTerm <* token AnonColon) <|> (term expression <* token AnonColon))
                          <*> ((emptyTerm <* token AnonColon) <|> (term expression <* token AnonColon) <|> (term expression <|> emptyTerm))
                          <*> (term expression <|> emptyTerm))

call :: Assignment Term
call = makeTerm <$> symbol Call <*> children (Expression.Call [] <$> term (identifier <|> expression) <*> (symbol ArgumentList *> children (manyTerm expression) <|> someTerm comprehension) <*> emptyTerm)

boolean :: Assignment Term
boolean =  makeTerm <$> token Grammar.True <*> pure Literal.true
       <|> makeTerm <$> token Grammar.False <*> pure Literal.false

none :: Assignment Term
none = makeTerm <$> symbol None <*> (Literal.Null <$ rawSource)

comprehension :: Assignment Term
comprehension =  makeTerm <$> symbol ListComprehension       <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol GeneratorExpression     <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol SetComprehension        <*> children (Declaration.Comprehension <$> term expression <*> expressions)
             <|> makeTerm <$> symbol DictionaryComprehension <*> children (Declaration.Comprehension <$> term expression <*> expressions)

forInClause :: Assignment Term
forInClause = symbol ForInClause *> children expressions

variables :: Assignment Term
variables = symbol Variables *> children expressions

ifClause :: Assignment Term
ifClause = symbol IfClause *> children expressions

conditionalExpression :: Assignment Term
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (flip Statement.If <$> term expression <*> term expression <*> expressions)


-- Helpers

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment Term -> Assignment [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

someTerm :: Assignment Term -> Assignment [Term]
someTerm term = some (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

term :: Assignment Term -> Assignment Term
term term = contextualize comment (postContextualize comment term)

-- | Match a left-associated infix chain of terms, optionally followed by comments. Like 'chainl1' but assigning comment nodes automatically.
chainl1Term :: Assignment Term -> Assignment (Term -> Term -> Term) -> Assignment Term
chainl1Term expr op = postContextualize (comment <|> symbol AnonLambda *> empty) expr `chainl1` op

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment Term -> Assignment b -> Assignment [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment Term
          -> Assignment Term
          -> [Assignment (Term -> Term -> Sum Syntax Term)]
          -> Assignment (Sum Syntax Term)
infixTerm = infixContext comment
