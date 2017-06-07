{-# LANGUAGE DataKinds, DeriveAnyClass, GeneralizedNewtypeDeriving, TypeOperators #-}
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
import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import GHC.Generics
import GHC.Stack
import Language.Python.Grammar as Grammar
import Prologue hiding (Location)
import qualified Term

type Syntax =
  '[ Comment.Comment
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
   , Statement.If
   , Statement.Return
   , Statement.Yield
   , Language.Python.Syntax.Ellipsis
   , Syntax.Empty
   , Syntax.Error Error
   , Syntax.Identifier
   , []
   ]

type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)

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
assignment :: HasCallStack => Assignment Grammar Term
assignment = makeTerm <$> symbol Module <*> children (many declaration)

declaration :: HasCallStack => Assignment Grammar Term
declaration = handleError $ comment <|> statement <|> expression

statement :: HasCallStack => Assignment Grammar Term
statement = assertStatement
          <|> assignment'
          <|> augmentedAssignment
          <|> expressionStatement
          <|> globalStatement
          <|> ifStatement
          <|> identifier
          <|> import'
          <|> importFrom
          <|> printStatement
          <|> returnStatement

expressionStatement :: HasCallStack => Assignment Grammar Term
expressionStatement = symbol ExpressionStatement *> children expression

expression :: HasCallStack => Assignment Grammar Term
expression = await
          <|> binaryOperator
          <|> booleanOperator
          <|> call
          <|> comparisonOperator
          <|> comprehension
          <|> conditionalExpression
          <|> dottedName
          <|> ellipsis
          <|> lambda
          <|> keywordIdentifier
          <|> literal
          <|> memberAccess
          <|> notOperator
          <|> subscript
          <|> statement
          <|> tuple
          <|> unaryOperator

dottedName :: HasCallStack => Assignment Grammar Term
dottedName = makeTerm <$> symbol DottedName <*> children (Expression.ScopeResolution <$> many expression)

ellipsis :: HasCallStack => Assignment Grammar Term
ellipsis = makeTerm <$> symbol Grammar.Ellipsis <*> (Language.Python.Syntax.Ellipsis <$ source)

comparisonOperator :: HasCallStack => Assignment Grammar Term
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

notOperator :: HasCallStack => Assignment Grammar Term
notOperator = makeTerm <$> symbol NotOperator <*> children (Expression.Not <$> expression)

keywordIdentifier :: HasCallStack => Assignment Grammar Term
keywordIdentifier = makeTerm <$> symbol KeywordIdentifier <*> children (Syntax.Identifier <$> source)

tuple :: HasCallStack => Assignment Grammar Term
tuple = makeTerm <$> symbol Tuple <*> children (Literal.Tuple <$> (many expression))

-- TODO: Consider flattening single element lists
expressionList :: HasCallStack => Assignment Grammar Term
expressionList = makeTerm <$> symbol ExpressionList <*> children (many expression)

unaryOperator :: HasCallStack => Assignment Grammar Term
unaryOperator = symbol UnaryOperator >>= \ location -> arithmetic location <|> bitwise location <|> children ( symbol AnonPlus *> expression )
  where
    arithmetic location = makeTerm location . Expression.Negate <$> children ( symbol AnonMinus *> expression )
    bitwise location    = makeTerm location . Expression.Complement <$> children ( symbol AnonTilde *> expression )

binaryOperator :: HasCallStack => Assignment Grammar Term
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

booleanOperator :: HasCallStack => Assignment Grammar Term
booleanOperator = makeTerm <$> symbol BooleanOperator <*> children ( expression >>= booleanOperator' )
  where
    booleanOperator' lexpression =  symbol AnonAnd *> (Expression.And lexpression <$> expression)
                                <|> symbol AnonOr *> (Expression.Or lexpression <$> expression)

assignment' :: HasCallStack => Assignment Grammar Term
assignment' =
  makeTerm <$> symbol Assignment <*> children (Statement.Assignment <$> expressionList <*> rvalue)

augmentedAssignment :: HasCallStack => Assignment Grammar Term
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

yield :: HasCallStack => Assignment Grammar Term
yield = makeTerm <$> symbol Yield <*> (Statement.Yield <$> children ( expression <|> expressionList <|> emptyTerm ))

rvalue :: HasCallStack => Assignment Grammar Term
rvalue  = expressionList <|> assignment' <|> augmentedAssignment <|> yield

identifier :: HasCallStack => Assignment Grammar Term
identifier = makeTerm <$> symbol Identifier <*> (Syntax.Identifier <$> source)

literal :: HasCallStack => Assignment Grammar Term
literal = string <|> integer <|> float <|> boolean <|> none <|> concatenatedString <|> list' <|> dictionary <|> set

set :: HasCallStack => Assignment Grammar Term
set = makeTerm <$> symbol Set <*> children (Literal.Set <$> many expression)

dictionary :: HasCallStack => Assignment Grammar Term
dictionary = makeTerm <$> symbol Dictionary <*> children (Literal.Hash <$> many pairs)
  where pairs = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

list' :: HasCallStack => Assignment Grammar Term
list' = makeTerm <$> symbol List <*> children (Literal.Array <$> many expression)

-- TODO: Wrap `Literal.TextElement` with a `Literal.String`
string :: HasCallStack => Assignment Grammar Term
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

concatenatedString :: HasCallStack => Assignment Grammar Term
concatenatedString = makeTerm <$> symbol ConcatenatedString <*> children (Literal.TextElement . mconcat <$> many (symbol String *> source))

float :: HasCallStack => Assignment Grammar Term
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

integer :: HasCallStack => Assignment Grammar Term
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

comment :: HasCallStack => Assignment Grammar Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

-- TODO Possibly match against children for dotted name and identifiers
import' :: HasCallStack => Assignment Grammar Term
import' = makeTerm <$> symbol ImportStatement <*> children (Declaration.Import <$> many expression)

-- TODO Possibly match against children nodes
importFrom :: HasCallStack => Assignment Grammar Term
importFrom = makeTerm <$> symbol ImportFromStatement <*> children (Declaration.Import <$> many expression)

assertStatement :: HasCallStack => Assignment Grammar Term
assertStatement = makeTerm <$ symbol AssertStatement <*> location <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAssert <*> (Syntax.Identifier <$> source)) <*> many expression)

printStatement :: HasCallStack => Assignment Grammar Term
printStatement = do
  location <- symbol PrintStatement
  children $ do
    print <- printKeyword
    redirectCallTerm location print <|> printCallTerm location print
  where
    printKeyword = makeTerm <$> symbol AnonPrint <*> (Syntax.Identifier <$> source)
    redirectCallTerm location keyword = makeTerm location <$ symbol Chevron <*> (flip Redirect <$> children expression <*> printCallTerm location keyword)
    printCallTerm location keyword = makeTerm location . Expression.Call keyword <$> many expression

globalStatement :: HasCallStack => Assignment Grammar Term
globalStatement = makeTerm <$> symbol GlobalStatement <*> children (Expression.Call <$> (makeTerm <$> symbol AnonGlobal <*> (Syntax.Identifier <$> source)) <*> many identifier)

await :: HasCallStack => Assignment Grammar Term
await = makeTerm <$> symbol Await <*> children (Expression.Call <$> (makeTerm <$> symbol AnonAwait <*> (Syntax.Identifier <$> source)) <*> many expression)

returnStatement :: HasCallStack => Assignment Grammar Term
returnStatement = makeTerm <$> symbol ReturnStatement <*> (Statement.Return <$> children expressionList)


ifStatement :: HasCallStack => Assignment Grammar Term
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> expression <*> statement <*> (flip (foldr makeElif) <$> many elifClause <*> optionalElse))
  where elseClause = symbol ElseClause *> children statement
        elifClause = (,) <$ symbol ElifClause <*> location <*> children (Statement.If <$> expression <*> statement)
        optionalElse = fromMaybe <$> emptyTerm <*> optional elseClause
        makeElif (loc, makeIf) rest = makeTerm loc (makeIf rest)

memberAccess :: HasCallStack => Assignment Grammar Term
memberAccess = makeTerm <$> symbol Attribute <*> children (Expression.MemberAccess <$> expression <*> expression)

subscript :: HasCallStack => Assignment Grammar Term
subscript = makeTerm <$> symbol Subscript <*> children (Expression.Subscript <$> expression <*> many expression)

call :: HasCallStack => Assignment Grammar Term
call = makeTerm <$> symbol Call <*> children (Expression.Call <$> identifier <*> (symbol ArgumentList *> children (many expression)
                                                                                <|> some comprehension))

boolean :: HasCallStack => Assignment Grammar Term
boolean =  makeTerm <$> symbol Grammar.True  <*> (Literal.true <$ source)
       <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ source)

none :: HasCallStack => Assignment Grammar Term
none = makeTerm <$> symbol None <*> (Literal.Null <$ source)

lambda :: HasCallStack => Assignment Grammar Term
lambda = makeTerm <$> symbol Lambda <*> children (Declaration.Function <$> lambdaIdentifier <*> lambdaParameters <*> lambdaBody)
  where lambdaIdentifier = makeTerm <$> symbol AnonLambda <*> (Syntax.Identifier <$> source)
        lambdaParameters = many identifier
        lambdaBody = expression

comprehension :: HasCallStack => Assignment Grammar Term
comprehension =  makeTerm <$> symbol GeneratorExpression <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol ListComprehension <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol SetComprehension <*> children (comprehensionDeclaration expression)
             <|> makeTerm <$> symbol DictionaryComprehension <*> children (comprehensionDeclaration keyValue)
  where
    keyValue = makeTerm <$> location <*> (Literal.KeyValue <$> expression <*> expression)
    comprehensionDeclaration preceeding = Declaration.Comprehension <$> preceeding <* symbol Variables <*> children (many expression) <*> (flip (foldr makeComprehension) <$> many nestedComprehension <*> expression)
    makeComprehension (loc, makeRest) rest = makeTerm loc (makeRest rest)
    nestedComprehension = (,) <$> location <*> (Declaration.Comprehension <$> expression <* symbol Variables <*> children (many expression))

conditionalExpression :: HasCallStack => Assignment Grammar Term
conditionalExpression = makeTerm <$> symbol ConditionalExpression <*> children (expression >>= \ thenBranch -> expression >>= \ conditional -> Statement.If conditional thenBranch <$> (expression <|> emptyTerm))

makeTerm :: (HasCallStack, InUnion fs f) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree (a :< inj f)

emptyTerm :: HasCallStack => Assignment Grammar Term
emptyTerm = makeTerm <$> location <*> pure Syntax.Empty

handleError :: HasCallStack => Assignment Grammar Term -> Assignment Grammar Term
handleError = flip catchError $ \ error -> case errorCause error of
  UnexpectedEndOfInput _ -> throwError error
  _ -> makeTerm <$> location <*> (Syntax.Error error <$ source)
