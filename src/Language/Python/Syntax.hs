{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Python.Syntax
( assignment
, Syntax
, Grammar
, Error
, Term
) where

import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Generics
import GHC.Stack
import Language.Python.Grammar as Grammar
import Prologue hiding (Location)
import qualified Term

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.Comprehension
   , Declaration.Function
   , Declaration.Import
   , Declaration.Variable
   , Expression.Arithmetic
   , Expression.Boolean
   , Expression.Bitwise
   , Expression.Call
   , Expression.Comparison
   , Expression.ScopeResolution
   , Expression.MemberAccess
   , Expression.Subscript
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
   , Redirect
   , Statement.Assignment
   , Statement.Break
   , Statement.Catch
   , Statement.Continue
   , Statement.Else
   , Statement.Finally
   , Statement.ForEach
   , Statement.If
   , Statement.NoOp
   , Statement.Return
   , Statement.Throw
   , Statement.Try
   , Statement.While
   , Statement.Yield
   , Language.Python.Syntax.Ellipsis
   , Syntax.Empty
   , Syntax.Error Error
   , Syntax.Identifier
   , Type.Annotation
   , []
   ]

type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Ellipsis where liftEq = genericLiftEq
instance Show1 Ellipsis where liftShowsPrec = genericLiftShowsPrec


data Redirect a = Redirect !a !a
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment
assignment = makeTerm <$> symbol Module <*> children (many declaration)

declaration :: Assignment
declaration = handleError $ classDefinition <|> comment <|> functionDefinition <|> asyncFunctionDefinition <|> expression <|> statement

statement :: Assignment
statement = assertStatement
          <|> assignment'
          <|> augmentedAssignment
          <|> breakStatement
          <|> continueStatement
          <|> deleteStatement
          <|> exceptClause
          <|> execStatement
          <|> expressionStatement
          <|> finallyClause
          <|> forStatement
          <|> globalStatement
          <|> ifStatement
          <|> identifier
          <|> import'
          <|> importFrom
          <|> nonlocalStatement
          <|> passStatement
          <|> printStatement
          <|> raiseStatement
          <|> returnStatement
          <|> tryStatement
          <|> whileStatement

expressionStatement :: Assignment
expressionStatement = symbol ExpressionStatement *> children expression

expression :: Assignment
expression = await
          <|> binaryOperator
          <|> booleanOperator
          <|> call
          <|> comparisonOperator
          <|> comprehension
          <|> conditionalExpression
          <|> dottedName
          <|> ellipsis
          <|> expressionList
          <|> lambda
          <|> keywordIdentifier
          <|> literal
          <|> memberAccess
          <|> notOperator
          <|> subscript
          <|> statement
          <|> tuple
          <|> type'
          <|> typedParameter
          <|> unaryOperator

forStatement :: Assignment
forStatement = symbol ForStatement >>= \ loc -> children (make loc <$> (makeTerm <$> symbol Variables <*> children (many expression)) <*> expressionList <*> (makeTerm <$> location <*> many expression) <*> (optional (makeTerm <$> symbol ElseClause <*> children (many declaration))))
  where
    make loc variables expressionList expressions elseClause = case elseClause of
      Nothing -> makeTerm loc $ (Statement.ForEach variables expressionList expressions)
      Just a -> makeTerm loc $ (Statement.Else (makeTerm loc $ Statement.ForEach variables expressionList expressions) a)

-- TODO: Assign while else clauses
whileStatement :: Assignment
whileStatement = makeTerm <$> symbol WhileStatement <*> children (Statement.While <$> expression <*> (makeTerm <$> location <*> many expression))

-- TODO: Assign try else clauses
tryStatement :: Assignment
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> expression <*> (many expression))

functionDefinition :: Assignment
functionDefinition =  symbol FunctionDefinition >>= \ loc -> children (make loc <$> identifier <*> (symbol Parameters *> children (many expression)) <*> (optional (symbol Type *> children expression)) <*> (makeTerm <$> location <*> many declaration))
  where
    make location functionName' functionParameters ty functionBody = case ty of
      Nothing -> makeTerm location $ Type.Annotation (makeTerm location $ Declaration.Function functionName' functionParameters functionBody) (makeTerm location Syntax.Empty)
      Just a -> makeTerm location $ Type.Annotation (makeTerm location $ Declaration.Function functionName' functionParameters functionBody) a

asyncFunctionDefinition :: Assignment
asyncFunctionDefinition = symbol AsyncFunctionDefinition >>= \ loc -> children (make loc <$> async' <*> identifier <*> (symbol Parameters *> children (many expression)) <*> (optional (symbol Type *> children expression)) <*> (makeTerm <$> location <*> many declaration))
  where
    make location async' functionName' functionParameters ty functionBody = case ty of
      Nothing -> makeTerm location $ Type.Annotation (makeTerm location $ Type.Annotation (makeTerm location $ Declaration.Function functionName' functionParameters functionBody) (makeTerm location Syntax.Empty)) async'
      Just a -> makeTerm location $ Type.Annotation (makeTerm location $ Type.Annotation (makeTerm location $ Declaration.Function functionName' functionParameters functionBody) a) async'

async' :: Assignment
async' = makeTerm <$> symbol AnonAsync <*> (Syntax.Identifier <$> source)

classDefinition :: Assignment
classDefinition = makeTerm <$> symbol ClassDefinition <*> children (Declaration.Class <$> identifier <*> argumentList <*> (many declaration))
  where argumentList = symbol ArgumentList *> children (many expression)
                    <|> pure []

typedParameter :: Assignment
typedParameter = makeTerm <$> symbol TypedParameter <*> children (Type.Annotation <$> identifier <*> type')

type' :: Assignment
type' = symbol Type *> children expression

exceptClause :: Assignment
exceptClause = makeTerm <$> symbol ExceptClause <*> children (Statement.Catch <$> optional (makeTerm <$> location <*> many expression <* symbol AnonColon) <*> expression)

finallyClause :: Assignment
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> expression)

dottedName :: Assignment
dottedName = makeTerm <$> symbol DottedName <*> children (Expression.ScopeResolution <$> many expression)

ellipsis :: Assignment
ellipsis = makeTerm <$> symbol Grammar.Ellipsis <*> (Language.Python.Syntax.Ellipsis <$ source)

comparisonOperator :: Assignment
comparisonOperator = symbol ComparisonOperator >>= \ loc -> children (expression >>= \ lexpression -> makeComparison loc lexpression)
  where
    makeComparison loc lexpression =  makeTerm loc <$ symbol AnonLAngle      <*> (Expression.LessThan lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonLAngleEqual <*> (Expression.LessThanEqual lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonRAngle      <*> (Expression.GreaterThan lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonRAngleEqual <*> (Expression.GreaterThanEqual lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonEqualEqual  <*> (Expression.Equal lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonBangEqual   <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> expression)))
                                  <|> makeTerm loc <$ symbol AnonNot         <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Member lexpression <$> expression)))
                                  <|> makeTerm loc <$ symbol AnonIn          <*> (Expression.Member lexpression <$> expression)
                                                    -- source is used here to push the cursor to the next node to enable matching against `AnonNot`
                                  <|> symbol AnonIs *> source *> (symbol AnonNot *> (makeTerm loc <$> Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> expression)))
                                                                <|> (makeTerm loc <$> Expression.Equal lexpression <$> expression))

notOperator :: Assignment
notOperator = makeTerm <$> symbol NotOperator <*> children (Expression.Not <$> expression)

keywordIdentifier :: Assignment
keywordIdentifier = makeTerm <$> symbol KeywordIdentifier <*> children (Syntax.Identifier <$> source)

tuple :: Assignment
tuple = makeTerm <$> symbol Tuple <*> children (Literal.Tuple <$> (many expression))

-- TODO: Consider flattening single element lists
expressionList :: Assignment
expressionList = makeTerm <$> symbol ExpressionList <*> children (many expression)

unaryOperator :: Assignment
unaryOperator = symbol UnaryOperator >>= \ location -> arithmetic location <|> bitwise location <|> children ( symbol AnonPlus *> expression )
  where
    arithmetic location = makeTerm location . Expression.Negate <$> children ( symbol AnonMinus *> expression )
    bitwise location    = makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )

binaryOperator :: Assignment
binaryOperator = symbol BinaryOperator >>= \ location -> children (expression >>= \ lexpression ->
      makeTerm location <$> arithmetic lexpression
  <|> makeTerm location <$> bitwise lexpression)
  where
    arithmetic lexpression =  symbol AnonPlus *> (Expression.Plus lexpression <$> expression)
                          <|> symbol AnonMinus *> (Expression.Minus lexpression <$> expression)
                          <|> symbol AnonStar *> (Expression.Times lexpression <$> expression)
                          <|> symbol AnonSlash *> (Expression.DividedBy lexpression <$> expression)
                          <|> symbol AnonSlashSlash *> (Expression.DividedBy lexpression <$> expression)
                          <|> symbol AnonPercent *> (Expression.Modulo lexpression <$> expression)
                          <|> symbol AnonStarStar *> (Expression.Power lexpression <$> expression)
    bitwise lexpression =  symbol AnonPipe *> (Expression.BOr lexpression <$> expression)
                       <|> symbol AnonAmpersand *> (Expression.BAnd lexpression <$> expression)
                       <|> symbol AnonCaret *> (Expression.BXOr lexpression <$> expression)
                       <|> symbol AnonLAngleLAngle *> (Expression.LShift lexpression <$> expression)
                       <|> symbol AnonRAngleRAngle *> (Expression.RShift lexpression <$> expression)

booleanOperator :: Assignment
booleanOperator = makeTerm <$> symbol BooleanOperator <*> children ( expression >>= booleanOperator' )
  where
    booleanOperator' lexpression =  symbol AnonAnd *> (Expression.And lexpression <$> expression)
                                <|> symbol AnonOr *> (Expression.Or lexpression <$> expression)

assignment' :: Assignment
assignment' =
  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> expressionList <*> rvalue)

augmentedAssignment :: Assignment
augmentedAssignment = makeTerm <$> symbol AugmentedAssignment <*> children (expressionList >>= \ lvalue -> Statement.Assignment lvalue <$>
         (makeTerm <$> symbol AnonPlusEqual               <*> (Expression.Plus lvalue      <$> rvalue)
      <|> makeTerm <$> symbol AnonMinusEqual              <*> (Expression.Minus lvalue     <$> rvalue)
      <|> makeTerm <$> symbol AnonStarEqual               <*> (Expression.Times lvalue     <$> rvalue)
      <|> makeTerm <$> symbol AnonStarStarEqual           <*> (Expression.Power lvalue     <$> rvalue)
      <|> makeTerm <$> symbol AnonSlashEqual              <*> (Expression.DividedBy lvalue <$> rvalue)
      <|> makeTerm <$> symbol AnonSlashSlashEqual         <*> (Expression.DividedBy lvalue <$> rvalue)
      <|> makeTerm <$> symbol AnonPipeEqual               <*> (Expression.BOr lvalue       <$> rvalue)
      <|> makeTerm <$> symbol AnonAmpersandEqual          <*> (Expression.BAnd lvalue      <$> rvalue)
      <|> makeTerm <$> symbol AnonPercentEqual            <*> (Expression.Modulo lvalue    <$> rvalue)
      <|> makeTerm <$> symbol AnonRAngleRAngleEqual       <*> (Expression.RShift lvalue    <$> rvalue)
      <|> makeTerm <$> symbol AnonLAngleLAngleEqual       <*> (Expression.LShift lvalue    <$> rvalue)
      <|> makeTerm <$> symbol AnonCaretEqual              <*> (Expression.BXOr lvalue      <$> rvalue)))

yield :: Assignment
yield = makeTerm <$> symbol Yield <*> (Statement.Yield <$> children ( expression <|> expressionList <|> emptyTerm ))

rvalue :: Assignment
rvalue  = expressionList <|> assignment' <|> augmentedAssignment <|> yield

identifier :: Assignment
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

literal :: Assignment
literal = string <|> integer <|> float <|> boolean <|> none <|> concatenatedString <|> list' <|> dictionary <|> set

set :: Assignment
set = makeTerm <$> symbol Set <*> children (Literal.Set <$> many expression)

dictionary :: Assignment
dictionary = makeTerm <$> symbol Dictionary <*> children (Literal.Hash <$> many pairs)
  where pairs = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

list' :: Assignment
list' = makeTerm <$> symbol List <*> children (Literal.Array <$> many expression)

-- TODO: Wrap `Literal.TextElement` with a `Literal.String`
string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

concatenatedString :: Assignment
concatenatedString = makeTerm <$> symbol ConcatenatedString <*> children (Literal.TextElement . mconcat <$> many (symbol String *> source))

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

-- TODO Possibly match against children for dotted name and identifiers
import' :: Assignment
import' = makeTerm <$> symbol ImportStatement <*> children (Declaration.Import <$> many expression)

-- TODO Possibly match against children nodes
importFrom :: Assignment
importFrom = makeTerm <$> symbol ImportFromStatement <*> children (Declaration.Import <$> many expression)

statements :: Assignment
statements = makeTerm <$> location <*> many statement

assertStatement :: Assignment
assertStatement = makeTerm <$ symbol AssertStatement <*> location <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAssert <*> (Syntax.Identifier <$> source)) <*> many expression)

printStatement :: Assignment
printStatement = do
  location <- symbol PrintStatement
  children $ do
    print <- printKeyword
    redirectCallTerm location print <|> printCallTerm location print
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier <$> source)
    redirectCallTerm location keyword = makeTerm location <$ symbol Chevron <*> (flip Redirect <$> children expression <*> printCallTerm location keyword)
    printCallTerm location keyword = makeTerm location . Expression.Call keyword <$> many expression

nonlocalStatement :: Assignment
nonlocalStatement = makeTerm <$> symbol NonlocalStatement <*> children (Expression.Call <$> (makeTerm <$> symbol AnonNonlocal <*> (Syntax.Identifier <$> source)) <*> many identifier)

globalStatement :: Assignment
globalStatement = makeTerm <$> symbol GlobalStatement <*> children (Expression.Call <$> (makeTerm <$> symbol AnonGlobal <*> (Syntax.Identifier <$> source)) <*> many identifier)

await :: Assignment
await = makeTerm <$> symbol Await <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAwait <*> (Syntax.Identifier <$> source)) <*> many expression)

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expressionList <|> emptyTerm))

deleteStatement :: Assignment
deleteStatement = makeTerm <$> symbol DeleteStatement <*> children (Expression.Call <$> deleteIdentifier <* symbol ExpressionList <*> children (many expression))
  where deleteIdentifier = makeTerm <$> symbol AnonDel <*> (Syntax.Identifier <$> source)

raiseStatement :: Assignment
raiseStatement = makeTerm <$> symbol RaiseStatement <*> children (Statement.Throw <$> (makeTerm <$> location <*> many expression))

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> expression <*> statement <*> (flip (foldr makeElif) <$> many elifClause <*> optionalElse))
  where elseClause = symbol ElseClause *> children statement
        elifClause = (,) <$ symbol ElifClause <*> location <*> children (Statement.If <$> expression <*> statement)
        optionalElse = fromMaybe <$> emptyTerm <*> optional elseClause
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)

execStatement :: Assignment
execStatement = makeTerm <$> symbol ExecStatement <*> children (Expression.Call <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)) <*> many (string <|> expression))

passStatement :: Assignment
passStatement = makeTerm <$> symbol PassStatement <*> (Statement.NoOp <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)))

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> (Statement.Break <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)))

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> (Statement.Continue <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)))

memberAccess :: Assignment
memberAccess = makeTerm <$> symbol Attribute <*> children (Expression.MemberAccess <$> expression <*> expression)

subscript :: Assignment
subscript = makeTerm <$> symbol Subscript <*> children (Expression.Subscript <$> expression <*> many expression)

call :: Assignment
call = makeTerm <$> symbol Call <*> children (Expression.Call <$> identifier <*> (symbol ArgumentList *> children (many expression)
                                                                                <|> some comprehension))

boolean :: Assignment
boolean =  makeTerm <$> symbol Grammar.True  <*> (Literal.true <$ source)
       <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

none :: Assignment
none = makeTerm <$> symbol None <*> (Literal.Null <$ source)

lambda :: Assignment
lambda = symbol Lambda >>= \ location -> children $ do
  lambdaIdentifier <- makeTerm <$> symbol AnonLambda <*> (Syntax.Identifier <$> source)
  lambdaParameters <- many identifier
  lambdaBody <- expression
  pure $ makeTerm location $ Type.Annotation (makeTerm location (Declaration.Function lambdaIdentifier lambdaParameters lambdaBody)) (makeTerm location Syntax.Empty)

comprehension :: Assignment
comprehension =  makeTerm <$> symbol GeneratorExpression <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol ListComprehension <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol SetComprehension <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol DictionaryComprehension <*> children (comprehensionDeclaration keyValue)
  where
    keyValue = makeTerm <$> location <*> (Literal.KeyValue <$> expression <*> expression)
    comprehensionDeclaration preceeding = Declaration.Comprehension <$> preceeding <* symbol Variables <*> children (many expression) <*> (flip (foldr makeComprehension) <$> many nestedComprehension <*> expression)
    makeComprehension (loc, makeRest) rest = makeTerm loc (makeRest rest)
    nestedComprehension = (,) <$> location <*> (Declaration.Comprehension <$> expression <* symbol Variables <*> children (many expression))

conditionalExpression :: Assignment
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (expression >>= \ thenBranch -> expression >>= \ conditional -> Statement.If conditional thenBranch <$> (expression <|> emptyTerm))

makeTerm :: (HasCallStack, f :< fs) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree (a :< inj f)

emptyTerm :: Assignment
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty

handleError :: Assignment -> Assignment
handleError = flip catchError $ \ error -> case errorCause error of
  UnexpectedEndOfInput _ -> throwError error
  _ -> makeTerm <$> location <*> (Syntax.Error error <$ source)
