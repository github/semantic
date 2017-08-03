{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Python.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Algorithm
import Control.Comonad.Cofree (Cofree(..))
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Maybe (fromMaybe)
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
import qualified Term

type Syntax =
  '[ Comment.Comment
   , Declaration.Class
   , Declaration.Comprehension
   , Declaration.Decorator
   , Declaration.Function
   , Declaration.Import
   , Declaration.Variable
   , Expression.Arithmetic
   , Expression.Boolean
   , Expression.Bitwise
   , Expression.Call
   , Expression.Comparison
   , Expression.Enumeration
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
   , Statement.Let
   , Statement.NoOp
   , Statement.Return
   , Statement.Throw
   , Statement.Try
   , Statement.While
   , Statement.Yield
   , Language.Python.Syntax.Ellipsis
   , Syntax.Empty
   , Syntax.Error
   , Syntax.Identifier
   , Syntax.Program
   , Type.Annotation
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Ellipsis where liftEq = genericLiftEq
instance Show1 Ellipsis where liftShowsPrec = genericLiftShowsPrec


data Redirect a = Redirect !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment
assignment =
      makeTerm <$> symbol Module <*> children (Syntax.Program <$> many expression)
  <|> parseError

expression :: Assignment
expression =
      argument
  <|> assertStatement
  <|> assignment'
  <|> await
  <|> binaryOperator
  <|> booleanOperator
  <|> breakStatement
  <|> call
  <|> classDefinition
  <|> comment
  <|> comparisonOperator
  <|> comprehension
  <|> conditionalExpression
  <|> continueStatement
  <|> decoratedDefinition
  <|> deleteStatement
  <|> dottedName
  <|> ellipsis
  <|> exceptClause
  <|> execStatement
  <|> expressionList
  <|> expressionStatement
  <|> finallyClause
  <|> forStatement
  <|> functionDefinition
  <|> globalStatement
  <|> identifier
  <|> ifStatement
  <|> import'
  <|> identifier
  <|> literal
  <|> memberAccess
  <|> nonlocalStatement
  <|> notOperator
  <|> parameter
  <|> parseError
  <|> passStatement
  <|> printStatement
  <|> raiseStatement
  <|> returnStatement
  <|> slice
  <|> subscript
  <|> tryStatement
  <|> tuple
  <|> type'
  <|> unaryOperator
  <|> whileStatement
  <|> withStatement
  <|> yield

expressions :: Assignment
expressions = makeTerm <$> location <*> many expression

literal :: Assignment
literal =  boolean
       <|> concatenatedString
       <|> dictionary
       <|> float
       <|> integer
       <|> list'
       <|> none
       <|> set
       <|> string
       <|> parseError

expressionStatement :: Assignment
expressionStatement = mk <$> symbol ExpressionStatement <*> children (some expression)
  where mk _ [child] = child
        mk location children = makeTerm location children

expressionList :: Assignment
expressionList = mk <$> symbol ExpressionList <*> children (some expression)
  where mk _ [child] = child
        mk location children = makeTerm location children

argument :: Assignment
argument = makeTerm <$> symbol ListSplatArgument <*> (Syntax.Identifier <$> source)
        <|> makeTerm <$> symbol DictionarySplatArgument <*> (Syntax.Identifier <$> source)
        <|> makeTerm <$> symbol KeywordArgument <*> children (Statement.Assignment <$> expression <*> expression)

parameter :: Assignment
parameter =  makeTerm <$> symbol DefaultParameter <*> children (Statement.Assignment <$> expression <*> expression)
         <|> makeTerm <$> symbol ListSplatParameter <*> (Syntax.Identifier <$> source)
         <|> makeTerm <$> symbol DictionarySplatParameter <*> (Syntax.Identifier <$> source)
         <|> makeTerm <$> symbol TypedParameter <*> children (Type.Annotation <$> expression <*> type')
         <|> makeAnnotation <$> symbol TypedDefaultParameter <*> children ((,,) <$> expression <*> expression <*> expression)
  where
    makeAnnotation loc (identifier', type', value') = makeTerm loc (Type.Annotation (makeAssignment loc identifier' value') type')
    makeAssignment loc identifier' value' = makeTerm loc (Statement.Assignment identifier' value')

decoratedDefinition :: Assignment
decoratedDefinition = symbol DecoratedDefinition *> children (makeDecorator <$> partialDecorator <*> (flip (foldr makeDecorator) <$> many partialDecorator <*> expression))
  where
    makeDecorator (loc, partialDecorator') next = makeTerm loc (partialDecorator' next)
    partialDecorator = (,) <$> symbol Decorator <*> children decorator'
    decorator' = Declaration.Decorator <$> expression <*> ((symbol ArgumentList *> children (many expression <|> many emptyTerm)) <|> many emptyTerm)

withStatement :: Assignment
withStatement = symbol WithStatement >>= \ loc -> children (mk loc <$> some with)
  where
    mk _ [child] = child
    mk l children = makeTerm l children
    with = makeTerm <$> location <*> (uncurry (flip Statement.Let) <$> withItem <*> expressions)
    withItem = symbol WithItem *> children ((,) <$> expression <*> (expression <|> emptyTerm))

forStatement :: Assignment
forStatement = symbol ForStatement >>= \ loc -> children (make loc <$> (makeTerm <$> symbol Variables <*> children (many expression)) <*> expressionList <*> expressions <*> optional (makeTerm <$> symbol ElseClause <*> children (many expression)))
  where
    make loc binding subject body forElseClause = case forElseClause of
      Nothing -> makeTerm loc (Statement.ForEach binding subject body)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.ForEach binding subject body) a)

whileStatement :: Assignment
whileStatement = symbol WhileStatement >>= \ loc -> children (make loc <$> expression <*> expressions <*> optional (makeTerm <$> symbol ElseClause <*> children (many expression)))
  where
    make loc whileCondition whileBody whileElseClause = case whileElseClause of
      Nothing -> makeTerm loc (Statement.While whileCondition whileBody)
      Just a -> makeTerm loc (Statement.Else (makeTerm loc $ Statement.While whileCondition whileBody) a)

tryStatement :: Assignment
tryStatement = makeTerm <$> symbol TryStatement <*> children (Statement.Try <$> expression <*> many (expression <|> elseClause))
  where elseClause = makeTerm <$> symbol ElseClause <*> children (Statement.Else <$> emptyTerm <*> (makeTerm <$> location <*> many expression))

exceptClause :: Assignment
exceptClause = makeTerm <$> symbol ExceptClause <*> children
  (Statement.Catch <$> ((makeTerm <$> location <*> (uncurry (flip Statement.Let) <$> ((,) <$> expression <* symbol AnonAs <*> expression) <*> emptyTerm))
                      <|> makeTerm <$> location <*> many expression)
                   <*> expressions)

functionDefinition :: Assignment
functionDefinition =  (symbol FunctionDefinition >>= \ loc -> children (makeFunctionDeclaration loc <$> expression <*> (symbol Parameters *> children (many expression)) <*> optional (symbol Type *> children expression) <*> expressions))
                  <|> (symbol AsyncFunctionDefinition >>= \ loc -> children (makeAsyncFunctionDeclaration loc <$> async' <*> expression <*> (symbol Parameters *> children (many expression)) <*> optional (symbol Type *> children expression) <*> expressions))
                  <|> (symbol Lambda >>= \ loc -> children (makeFunctionDeclaration loc <$> (makeTerm <$> symbol AnonLambda <*> (Syntax.Identifier <$> source)) <*> ((symbol LambdaParameters *> children (many expression)) <|> pure []) <*> optional (symbol Type *> children expression) <*> expressions))
  where
    makeFunctionDeclaration loc functionName' functionParameters ty functionBody = makeTerm loc $ Type.Annotation (makeTerm loc $ Declaration.Function functionName' functionParameters functionBody) (maybe (makeTerm loc Syntax.Empty) id ty)
    makeAsyncFunctionDeclaration loc async' functionName' functionParameters ty functionBody = makeTerm loc $ Type.Annotation (makeTerm loc $ Type.Annotation (makeTerm loc $ Declaration.Function functionName' functionParameters functionBody) (maybe (makeTerm loc Syntax.Empty) id ty)) async'

async' :: Assignment
async' = makeTerm <$> symbol AnonAsync <*> (Syntax.Identifier <$> source)

classDefinition :: Assignment
classDefinition = makeTerm <$> symbol ClassDefinition <*> children (Declaration.Class <$> expression <*> argumentList <*> many expression)
  where argumentList = symbol ArgumentList *> children (many expression)
                    <|> pure []

type' :: Assignment
type' = symbol Type *> children expression

finallyClause :: Assignment
finallyClause = makeTerm <$> symbol FinallyClause <*> children (Statement.Finally <$> expressions)

dottedName :: Assignment
dottedName = makeTerm <$> symbol DottedName <*> children (Expression.ScopeResolution <$> many expression)

ellipsis :: Assignment
ellipsis = makeTerm <$> symbol Grammar.Ellipsis <*> (Language.Python.Syntax.Ellipsis <$ source)

comparisonOperator :: Assignment
comparisonOperator = symbol ComparisonOperator >>= \ loc -> children (expression >>= \ lexpression -> makeComparison loc lexpression)
  where
    makeComparison loc lexpression =  makeTerm loc <$ symbol AnonLAngle       <*> (Expression.LessThan lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonLAngleEqual  <*> (Expression.LessThanEqual lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonRAngle       <*> (Expression.GreaterThan lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonRAngleEqual  <*> (Expression.GreaterThanEqual lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonEqualEqual   <*> (Expression.Equal lexpression <$> expression)
                                  <|> makeTerm loc <$ symbol AnonBangEqual    <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> expression)))
                                  <|> makeTerm loc <$ symbol AnonLAngleRAngle <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> expression)))
                                  <|> makeTerm loc <$ symbol AnonNot          <*> (Expression.Not <$> (makeTerm <$> location <*> (Expression.Member lexpression <$> expression)))
                                  <|> makeTerm loc <$ symbol AnonIn           <*> (Expression.Member lexpression <$> expression)
                                                    -- source is used here to push the cursor to the next node to enable matching against `AnonNot`
                                  <|> symbol AnonIs *> source *> (symbol AnonNot *> (makeTerm loc <$> Expression.Not <$> (makeTerm <$> location <*> (Expression.Equal lexpression <$> expression)))
                                                                <|> (makeTerm loc <$> Expression.Equal lexpression <$> expression))

notOperator :: Assignment
notOperator = makeTerm <$> symbol NotOperator <*> children (Expression.Not <$> expression)

tuple :: Assignment
tuple = makeTerm <$> symbol Tuple <*> children (Literal.Tuple <$> many expression)

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
assignment' =  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> expressionList <*> rvalue)
           <|> makeTerm <$> symbol AugmentedAssignment <*> children (expressionList >>= \ lvalue -> Statement.Assignment lvalue <$>
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
  where
    rvalue = expressionList <|> assignment' <|> yield

yield :: Assignment
yield = makeTerm <$> symbol Yield <*> (Statement.Yield <$> children ( expression <|> emptyTerm ))

identifier :: Assignment
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

set :: Assignment
set = makeTerm <$> symbol Set <*> children (Literal.Set <$> many expression)

dictionary :: Assignment
dictionary = makeTerm <$> symbol Dictionary <*> children (Literal.Hash <$> many (pair <|> comment))
  where pair = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

list' :: Assignment
list' = makeTerm <$> symbol List <*> children (Literal.Array <$> many expression)

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

import' :: Assignment
import' =  makeTerm <$> symbol ImportStatement <*> children (Declaration.Import <$> many expression)
       <|> makeTerm <$> symbol ImportFromStatement <*> children (Declaration.Import <$> many expression)
       <|> makeTerm <$> symbol AliasedImport <*> children (flip Statement.Let <$> expression <*> expression <*> emptyTerm)
       <|> makeTerm <$> symbol WildcardImport <*> (Syntax.Identifier <$> source)
       <|> parseError

assertStatement :: Assignment
assertStatement = makeTerm <$ symbol AssertStatement <*> location <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAssert <*> (Syntax.Identifier <$> source)) <*> many expression <*> emptyTerm)

printStatement :: Assignment
printStatement = do
  location <- symbol PrintStatement
  children $ do
    print <- printKeyword
    redirectCallTerm location print <|> printCallTerm location print
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier <$> source)
    redirectCallTerm location identifier = makeTerm location <$ symbol Chevron <*> (flip Redirect <$> children expression <*> printCallTerm location identifier)
    printCallTerm location identifier = makeTerm location <$> (Expression.Call identifier <$> many expression <*> emptyTerm)

nonlocalStatement :: Assignment
nonlocalStatement = makeTerm <$> symbol NonlocalStatement <*> children (Expression.Call <$> (makeTerm <$> symbol AnonNonlocal <*> (Syntax.Identifier <$> source)) <*> many expression <*> emptyTerm)

globalStatement :: Assignment
globalStatement = makeTerm <$> symbol GlobalStatement <*> children (Expression.Call <$> (makeTerm <$> symbol AnonGlobal <*> (Syntax.Identifier <$> source)) <*> many expression <*> emptyTerm)

await :: Assignment
await = makeTerm <$> symbol Await <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAwait <*> (Syntax.Identifier <$> source)) <*> many expression <*> emptyTerm)

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expressionList <|> emptyTerm))

deleteStatement :: Assignment
deleteStatement = makeTerm <$> symbol DeleteStatement <*> children (Expression.Call <$> deleteIdentifier <* symbol ExpressionList <*> children (many expression) <*> emptyTerm)
  where deleteIdentifier = makeTerm <$> symbol AnonDel <*> (Syntax.Identifier <$> source)

raiseStatement :: Assignment
raiseStatement = makeTerm <$> symbol RaiseStatement <*> children (Statement.Throw <$> expressions)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> expression <*> expressions <*> (flip (foldr makeElif) <$> many elifClause <*> optionalElse))
  where elseClause = symbol ElseClause *> children expressions
        elifClause = (,) <$ symbol ElifClause <*> location <*> children (Statement.If <$> expression <*> expressions)
        optionalElse = fromMaybe <$> emptyTerm <*> optional elseClause
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)

execStatement :: Assignment
execStatement = makeTerm <$> symbol ExecStatement <*> children (Expression.Call <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)) <*> many (string <|> expression) <*> emptyTerm)

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

slice :: Assignment
slice = makeTerm <$> symbol Slice <*> children
  (Expression.Enumeration <$> ((emptyTerm <* symbol AnonColon <* source) <|> (expression <* symbol AnonColon <* source))
                          <*> ((emptyTerm <* symbol AnonColon <* source) <|> (expression <* symbol AnonColon <* source) <|> (expression <|> emptyTerm))
                          <*> (expression <|> emptyTerm))

call :: Assignment
call = makeTerm <$> symbol Call <*> children (Expression.Call <$> expression <*> (symbol ArgumentList *> children (many expression)
                                                                                <|> some comprehension) <*> emptyTerm)

boolean :: Assignment
boolean =  makeTerm <$> symbol Grammar.True  <*> (Literal.true <$ source)
       <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

none :: Assignment
none = makeTerm <$> symbol None <*> (Literal.Null <$ source)

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
makeTerm a f = a :< inj f

emptyTerm :: Assignment
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty

parseError :: Assignment
parseError = makeTerm <$> symbol ParseError <*> (Syntax.Error [] <$ source)
