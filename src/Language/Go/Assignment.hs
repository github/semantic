{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TupleSections, TypeOperators #-}
module Language.Go.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Prologue
import Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import Data.Record
import Data.Syntax (contextualize, emptyTerm, parseError, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import Language.Go.Grammar as Grammar
import Language.Go.Syntax as Go.Syntax
import Language.Go.Type as Go.Type

type Syntax =
  '[ Comment.Comment
   , Declaration.Constructor
   , Declaration.Function
   , Declaration.Import
   , Declaration.ImportSymbol
   , Declaration.Method
   , Declaration.MethodSignature
   , Declaration.Module
   , Declaration.Type
   , Declaration.TypeAlias
   , Expression.Arithmetic
   , Expression.Bitwise
   , Expression.Boolean
   , Expression.Call
   , Expression.Comparison
   , Expression.Subscript
   , Statement.PostDecrement
   , Statement.PostIncrement
   , Expression.MemberAccess
   , Go.Syntax.Composite
   , Go.Syntax.DefaultPattern
   , Go.Syntax.Defer
   , Go.Syntax.Field
   , Go.Syntax.Go
   , Go.Syntax.Label
   , Go.Syntax.Receive
   , Go.Syntax.ReceiveOperator
   , Go.Syntax.Rune
   , Go.Syntax.Select
   , Go.Syntax.Send
   , Go.Syntax.Slice
   , Go.Syntax.TypeAssertion
   , Go.Syntax.TypeConversion
   , Go.Syntax.TypeSwitch
   , Go.Syntax.TypeSwitchGuard
   , Go.Syntax.Variadic
   , Go.Type.BidirectionalChannel
   , Go.Type.ReceiveChannel
   , Go.Type.SendChannel
   , Literal.Array
   , Literal.Complex
   , Literal.Float
   , Literal.Hash
   , Literal.Integer
   , Literal.KeyValue
   , Literal.Pointer
   , Literal.Reference
   , Literal.TextElement
   , Statement.Assignment
   , Statement.Break
   , Statement.Continue
   , Statement.For
   , Statement.ForEach
   , Statement.Goto
   , Statement.If
   , Statement.Match
   , Statement.NoOp
   , Statement.Pattern
   , Statement.Return
   , Syntax.Context
   , Syntax.Error
   , Syntax.Empty
   , Syntax.Identifier
   , Syntax.Program
   , Type.Annotation
   , Type.Array
   , Type.Function
   , Type.Interface
   , Type.Map
   , Type.Parenthesized
   , Type.Pointer
   , Type.Slice
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term


-- | Assignment from AST in Go's grammar onto a program in Go's syntax.
assignment :: Assignment
assignment = handleError program <|> parseError

program :: Assignment
program = makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> manyTerm expression)

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [ argumentList
  , assignment'
  , binaryExpression
  , block
  , breakStatement
  , callExpression
  , communicationClause
  , compositeLiteral
  , continueStatement
  , varDeclaration
  , varSpecification
  , decStatement
  , defaultCase
  , deferStatement
  , element
  , elseClause
  , emptyStatement
  , expressionCaseClause
  , expressionList
  , expressionSwitchStatement
  , fallThroughStatement
  , fieldDeclaration
  , fieldDeclarationList
  , fieldIdentifier
  , floatLiteral
  , forStatement
  , functionDeclaration
  , goStatement
  , gotoStatement
  , ifInitializer
  , ifStatement
  , imaginaryLiteral
  , incStatement
  , identifier
  , importDeclaration
  , indexExpression
  , interpretedStringLiteral
  , intLiteral
  , keyedElement
  , labelName
  , labeledStatement
  , literalValue
  , methodDeclaration
  , methodSpec
  , methodSpecList
  , packageClause
  , packageIdentifier
  , parameterDeclaration
  , parameters
  , parenthesizedExpression
  , rawStringLiteral
  , receiveStatement
  , returnStatement
  , runeLiteral
  , selectStatement
  , selectorExpression
  , sendStatement
  , shortVarDeclaration
  , sliceExpression
  , unaryExpression
  , variadicArgument
  , variadicParameterDeclaration
  , types
  ]

types :: Assignment
types =
  choice [ arrayType
         , channelType
         , functionType
         , implicitLengthArrayType
         , interfaceType
         , mapType
         , parenthesizedType
         , pointerType
         , qualifiedType
         , sliceType
         , structType
         , typeAssertion
         , typeConversion
         , typeDeclaration
         , typeIdentifier
         , typeCase
         , typeCaseClause
         , typeSwitchGuard
         , typeSwitchStatement
         ]

identifiers :: Assignment
identifiers = makeTerm'' <$> location <*> manyTerm identifier

expressions :: Assignment
expressions = makeTerm'' <$> location <*> manyTerm expression


-- Literals

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

compositeLiteral :: Assignment
compositeLiteral = makeTerm <$> symbol CompositeLiteral <*> children (Go.Syntax.Composite <$> expression <*> expression)

element :: Assignment
element = symbol Element *> children expression

fieldIdentifier :: Assignment
fieldIdentifier = makeTerm <$> symbol FieldIdentifier <*> (Syntax.Identifier <$> source)

floatLiteral :: Assignment
floatLiteral = makeTerm <$> symbol FloatLiteral <*> (source >>= Literal.normalizeFloatString [Literal.padWithLeadingZero, Literal.dropAlphaSuffix])

identifier :: Assignment
identifier =  makeTerm <$> (symbol Identifier <|> symbol Identifier') <*> (Syntax.Identifier <$> source)

imaginaryLiteral :: Assignment
imaginaryLiteral = makeTerm <$> symbol ImaginaryLiteral <*> (Literal.Complex <$> source)

interpretedStringLiteral :: Assignment
interpretedStringLiteral = makeTerm <$> symbol InterpretedStringLiteral <*> (Literal.TextElement <$> source)

intLiteral :: Assignment
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

literalValue :: Assignment
literalValue = makeTerm <$> symbol LiteralValue <*> children (manyTerm expression)

packageIdentifier :: Assignment
packageIdentifier = makeTerm <$> symbol PackageIdentifier <*> (Syntax.Identifier <$> source)

parenthesizedType :: Assignment
parenthesizedType = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Type.Parenthesized <$> expression)

rawStringLiteral :: Assignment
rawStringLiteral = makeTerm <$> symbol RawStringLiteral <*> (Literal.TextElement <$> source)

runeLiteral :: Assignment
runeLiteral = makeTerm <$> symbol Grammar.RuneLiteral <*> (Go.Syntax.Rune <$> source)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol TypeIdentifier <*> (Syntax.Identifier <$> source)


-- Primitive Types

arrayType :: Assignment
arrayType = makeTerm <$> symbol ArrayType <*> children (Type.Array . Just <$> expression <*> expression)

channelType :: Assignment
channelType =  makeTerm' <$> symbol ChannelType <*> children (mkChannelType <$> optional (token AnonLAngleMinus) <* token AnonChan <*> optional (token AnonLAngleMinus) <*> expression)
  where
    mkChannelType :: Maybe a -> Maybe a -> b -> Union Syntax b
    mkChannelType receive send | Just _ <- receive = inj . Go.Type.ReceiveChannel
                               | Just _ <- send    = inj . Go.Type.SendChannel
                               | otherwise         = inj . Go.Type.BidirectionalChannel

fieldDeclaration :: Assignment
fieldDeclaration =  mkFieldDeclarationWithTag <$> symbol FieldDeclaration <*> children ((,,) <$> (manyTermsTill expression (void (symbol TypeIdentifier)) <|> (manyTerm expression)) <*> optional expression <*> optional expression)
  where
    mkFieldDeclarationWithTag loc (fields, type', tag) | Just ty <- type', Just tag' <- tag = makeTerm loc (Go.Syntax.Field [ty, tag'] (makeTerm loc fields))
                                                       | Just ty <- type'                   = makeTerm loc (Go.Syntax.Field [ty] (makeTerm loc fields))
                                                       | Just tag' <- tag                   = makeTerm loc (Go.Syntax.Field [tag'] (makeTerm loc fields))
                                                       | otherwise                          = makeTerm loc (Go.Syntax.Field [] (makeTerm loc fields))

fieldDeclarationList :: Assignment
fieldDeclarationList = symbol FieldDeclarationList *> children expressions

functionType :: Assignment
functionType = makeTerm <$> symbol FunctionType <*> children (Type.Function <$> manyTerm parameters <*> (expression <|> emptyTerm))

implicitLengthArrayType :: Assignment
implicitLengthArrayType = makeTerm <$> symbol ImplicitLengthArrayType <*> children (Type.Array Nothing <$> expression)

interfaceType :: Assignment
interfaceType = makeTerm <$> symbol InterfaceType <*> children (Type.Interface <$> manyTerm expression)

mapType :: Assignment
mapType = makeTerm <$> symbol MapType <*> children (Type.Map <$> expression <*> expression)

pointerType :: Assignment
pointerType = makeTerm <$> symbol PointerType <*> children (Type.Pointer <$> expression)

qualifiedType :: Assignment
qualifiedType = makeTerm <$> symbol QualifiedType <*> children (Expression.MemberAccess <$> expression <*> expression)

sliceType :: Assignment
sliceType = makeTerm <$> symbol SliceType <*> children (Type.Slice <$> expression)

structType :: Assignment
structType = makeTerm <$> symbol StructType <*> children (Declaration.Constructor <$> emptyTerm <*> manyTerm expression)

typeAlias :: Assignment
typeAlias = makeTerm <$> symbol TypeAlias <*> children (Declaration.TypeAlias [] <$> expression <*> expression)

typeDeclaration :: Assignment
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (manyTerm ( (makeTerm <$> symbol TypeSpec <*> children (Declaration.Type <$> typeIdentifier <*> expression))
                                                                            <|> typeAlias ))



-- Expressions

argumentList :: Assignment
argumentList = (symbol ArgumentList <|> symbol ArgumentList') *> children expressions

binaryExpression :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression expression
  [ (inj .) . Expression.Plus             <$ symbol AnonPlus
  , (inj .) . Expression.Minus            <$ symbol AnonMinus
  , (inj .) . Expression.Times            <$ symbol AnonStar
  , (inj .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inj .) . Expression.Modulo           <$ symbol AnonPercent
  , (inj .) . Expression.Or               <$ symbol AnonPipePipe
  , (inj .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inj .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inj .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inj .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inj .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (inj .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (inj .) . Expression.Equal            <$ symbol AnonEqualEqual
  , (inj .) . Expression.BOr              <$ symbol AnonPipe
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inj .) . Expression.BAnd             <$ symbol AnonAmpersandCaret
  , (inj .) . Expression.BXOr             <$ symbol AnonCaret
  , (inj .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inj .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  ])
  where
    invert cons a b = Expression.Not (makeTerm1 (cons a b))

block :: Assignment
block = symbol Block *> children expressions

defaultCase :: Assignment
defaultCase = makeTerm <$> symbol DefaultCase <*> children (Go.Syntax.DefaultPattern <$> (expressions <|> emptyTerm))

defaultExpressionCase :: Assignment
defaultExpressionCase = makeTerm <$> symbol DefaultCase <*> (Go.Syntax.DefaultPattern <$ source <*> (expressions <|> emptyTerm))

callExpression :: Assignment
callExpression = makeTerm <$> symbol CallExpression <*> children (Expression.Call <$> pure [] <*> expression <*> manyTerm expression <*> emptyTerm)

expressionCase :: Assignment
expressionCase = makeTerm <$> symbol ExpressionCase <*> (Statement.Pattern <$> children expressions <*> expressions)

expressionCaseClause :: Assignment
expressionCaseClause = symbol ExpressionCaseClause *> children (expressionCase <|> defaultExpressionCase)

expressionList :: Assignment
expressionList = symbol ExpressionList *> children expressions

expressionSwitchStatement :: Assignment
expressionSwitchStatement = makeTerm <$> symbol ExpressionSwitchStatement <*> children (Statement.Match <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ExpressionCaseClause)) <|> emptyTerm) <*> expressions)

fallThroughStatement :: Assignment
fallThroughStatement = makeTerm <$> symbol FallthroughStatement <*> (Statement.Pattern <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)) <*> emptyTerm)

functionDeclaration :: Assignment
functionDeclaration =  makeTerm <$> (symbol FunctionDeclaration <|> symbol FuncLiteral) <*> children (mkFunctionDeclaration <$> (term identifier <|> emptyTerm) <*> manyTerm parameters <*> (term types <|> term identifier <|> term returnParameters <|> emptyTerm) <*> (term block <|> emptyTerm))
  where
    mkFunctionDeclaration name' params' types' block' = Declaration.Function [types'] name' params' block'
    returnParameters = makeTerm <$> symbol ParameterList <*> children (manyTerm expression)

importDeclaration :: Assignment
importDeclaration = makeTerm'' <$> symbol ImportDeclaration <*> children (manyTerm (importSpec <|> importSpecList))
  where
    importSpec = makeTerm <$> symbol ImportSpec <*> children (namedImport <|> plainImport)
    namedImport = flip Declaration.Import <$> expression' <*> expression' <*> pure []
    plainImport = Declaration.Import <$> expression' <*> emptyTerm <*> pure []
    importSpecList = makeTerm <$> symbol ImportSpecList <*> children (manyTerm (importSpec <|> comment))

    expression' = expression <|> dotImport <|> blankImport
    dotImport = makeTerm <$> symbol Dot <*> (Syntax.Identifier <$> source)
    blankImport = makeTerm <$> symbol BlankIdentifier <*> (Syntax.Identifier <$> source)

indexExpression :: Assignment
indexExpression = makeTerm <$> symbol IndexExpression <*> children (Expression.Subscript <$> expression <*> manyTerm expression)

methodDeclaration :: Assignment
methodDeclaration = makeTerm <$> symbol MethodDeclaration <*> children (mkTypedMethodDeclaration <$> receiver <*> term fieldIdentifier <*> manyTerm parameters <*> ((makeTerm <$> location <*> (manyTermsTill expression (void (symbol Block)))) <|> emptyTerm) <*> (term block <|> emptyTerm))
  where
    receiver = symbol ParameterList *> children ((symbol ParameterDeclaration *> children expressions) <|> expressions)
    mkTypedMethodDeclaration receiver' name' parameters' type'' body' = Declaration.Method [type''] receiver' name' parameters' body'

methodSpec :: Assignment
methodSpec =  makeTerm <$> symbol MethodSpec <*> children (mkMethodSpec <$> expression <*> parameters <*> (expression <|> emptyTerm))
  where
    mkMethodSpec name' params optionalTypeLiteral = Declaration.MethodSignature [optionalTypeLiteral] name' [params]

methodSpecList :: Assignment
methodSpecList = symbol MethodSpecList *> children expressions

packageClause :: Assignment
packageClause = makeTerm <$> symbol PackageClause <*> children (Declaration.Module <$> expression <*> pure [])

parameters :: Assignment
parameters = symbol ParameterList *> children expressions

parameterDeclaration :: Assignment
parameterDeclaration = makeTerm <$> symbol ParameterDeclaration <*> children (manyTerm expression)

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

selectorExpression :: Assignment
selectorExpression = makeTerm <$> symbol SelectorExpression <*> children (Expression.MemberAccess <$> expression <*> expression)

sliceExpression :: Assignment
sliceExpression = makeTerm <$> symbol SliceExpression <*> children (Go.Syntax.Slice <$> expression <* token AnonLBracket <*> (emptyTerm <|> expression) <* token AnonColon <*> (expression <|> emptyTerm) <* optional (token AnonColon) <*> (expression <|> emptyTerm))

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol TypeAssertionExpression <*> children (Go.Syntax.TypeAssertion <$> expression <*> expression)

typeCase :: Assignment
typeCase = symbol TypeCase *> children expressions

typeCaseClause :: Assignment
typeCaseClause = makeTerm <$> symbol TypeCaseClause <*> children (Statement.Pattern <$> expression <*> expressions)

typeConversion :: Assignment
typeConversion = makeTerm <$> symbol TypeConversionExpression <*> children (Go.Syntax.TypeConversion <$> expression <*> expression)

typeSwitchGuard :: Assignment
typeSwitchGuard = makeTerm <$> symbol Grammar.TypeSwitchGuard <*> children (Go.Syntax.TypeSwitchGuard <$> expressions)

typeSwitchStatement :: Assignment
typeSwitchStatement = makeTerm <$> symbol TypeSwitchStatement <*> children (Go.Syntax.TypeSwitch <$> typeSwitchSubject <*> expressions)
  where
    typeSwitchSubject = makeTerm <$> location <*> manyTermsTill expression (void (symbol TypeCaseClause)) <|> emptyTerm

unaryExpression :: Assignment
unaryExpression = makeTerm' <$> symbol UnaryExpression <*> (  notExpression
                                                          <|> unaryMinus
                                                          <|> unaryAmpersand
                                                          <|> unaryReceive
                                                          <|> unaryPointer
                                                          <|> unaryComplement
                                                          <|> unaryPlus )
  where
    notExpression   = inj <$> (children (Expression.Not <$ symbol AnonBang <*> expression))
    unaryAmpersand  = inj <$> (children (Literal.Reference <$ symbol AnonAmpersand <*> expression))
    unaryComplement = inj <$> (children (Expression.Complement <$ symbol AnonCaret <*> expression))
    unaryMinus      = inj <$> (children (Expression.Negate <$ symbol AnonMinus <*> expression))
    unaryPlus       =          children (symbol AnonPlus *> (Term.termOut <$> expression))
    unaryPointer    = inj <$> (children (Literal.Pointer <$ symbol AnonStar <*> expression))
    unaryReceive    = inj <$> (children (Go.Syntax.ReceiveOperator <$ symbol AnonLAngleMinus <*> expression))

varDeclaration :: Assignment
varDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

variadicArgument :: Assignment
variadicArgument = makeTerm <$> symbol VariadicArgument <*> children (Go.Syntax.Variadic <$> pure [] <*> expression)

variadicParameterDeclaration :: Assignment
variadicParameterDeclaration =  makeTerm <$> symbol VariadicParameterDeclaration <*> children (flip Go.Syntax.Variadic <$> (expression <|> emptyTerm) <* token AnonDotDotDot <*> many expression)

varSpecification :: Assignment
varSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment <$> pure [] <*> (annotatedLHS <|> identifiers) <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation <$> (makeTerm <$> location <*> (manyTermsTill identifier (void (symbol TypeIdentifier)))) <*> expression)


-- Statements

assignment' :: Assignment
assignment' =  makeTerm' <$> symbol AssignmentStatement <*> children (infixTerm expressionList expressionList
                  [ assign                                   <$ symbol AnonEqual
                  , augmentedAssign Expression.Plus          <$ symbol AnonPlusEqual
                  , augmentedAssign Expression.Minus         <$ symbol AnonMinusEqual
                  , augmentedAssign Expression.Times         <$ symbol AnonStarEqual
                  , augmentedAssign Expression.DividedBy     <$ symbol AnonSlashEqual
                  , augmentedAssign Expression.BOr           <$ symbol AnonPipeEqual
                  , augmentedAssign Expression.BAnd          <$ symbol AnonAmpersandEqual
                  , augmentedAssign Expression.Modulo        <$ symbol AnonPercentEqual
                  , augmentedAssign Expression.RShift        <$ symbol AnonRAngleRAngleEqual
                  , augmentedAssign Expression.LShift        <$ symbol AnonLAngleLAngleEqual
                  , augmentedAssign Expression.BXOr          <$ symbol AnonCaretEqual
                  , augmentedAssign (invert Expression.BAnd) <$ symbol AnonAmpersandCaretEqual
                  ])
  where
    assign :: Term -> Term -> Union Syntax Term
    assign l r = inj (Statement.Assignment [] l r)

    augmentedAssign :: f :< Syntax => (Term -> Term -> f Term) -> Term -> Term -> Union Syntax Term
    augmentedAssign c l r = assign l (makeTerm1 (c l r))

    invert cons a b = Expression.Not (makeTerm1 (cons a b))

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (expression <|> emptyTerm))

communicationClause :: Assignment
communicationClause = makeTerm <$> symbol CommunicationClause <*> children (Statement.Pattern <$> (communicationCase <|> expression) <*> expressions)
  where
    communicationCase = symbol CommunicationCase *> children expression

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (expression <|> emptyTerm))

decStatement :: Assignment
decStatement = makeTerm <$> symbol DecStatement <*> children (Statement.PostDecrement <$> expression)

deferStatement :: Assignment
deferStatement = makeTerm <$> symbol DeferStatement <*> children (Go.Syntax.Defer <$> expression)

elseClause :: Assignment
elseClause = symbol ElseClause *> children expression

emptyStatement :: Assignment
emptyStatement = makeTerm <$> token EmptyStatement <*> (Statement.NoOp <$> emptyTerm)

forStatement :: Assignment
forStatement =  makeTerm' <$> symbol ForStatement <*> children (forClause <|> forSimpleClause <|> rangeClause)
  where
    forClause = inj <$> (symbol ForClause *> children (Statement.For <$> (expression <|> emptyTerm) <*> (expression <|> emptyTerm) <*> (expression <|> emptyTerm)) <*> expression)
    forSimpleClause = inj <$> (Statement.For <$> emptyTerm <*> (expression <|> emptyTerm) <*> emptyTerm <*> expression)
    rangeClause = inj <$> (symbol RangeClause *> children (Statement.ForEach <$> (expression <|> emptyTerm) <*> expression) <*> expression)

goStatement :: Assignment
goStatement = makeTerm <$> symbol GoStatement <*> children (Go.Syntax.Go <$> expression)

gotoStatement :: Assignment
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> expression)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol Block))) <*> expression <*> (expression <|> emptyTerm))

ifInitializer :: Assignment
ifInitializer = symbol IfInitializer *> children expression

incStatement :: Assignment
incStatement = makeTerm <$> symbol IncStatement <*> children (Statement.PostIncrement <$> expression)

keyedElement :: Assignment
keyedElement = makeTerm <$> symbol KeyedElement <*> children (Literal.KeyValue <$> expression <*> expression)

labelName :: Assignment
labelName = makeTerm <$> symbol LabelName <*> (Syntax.Identifier <$> source)

labeledStatement :: Assignment
labeledStatement = makeTerm <$> (symbol LabeledStatement <|> symbol LabeledStatement') <*> children (Go.Syntax.Label <$> expression <*> (expression <|> emptyTerm))

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expression <|> emptyTerm))

receiveStatement :: Assignment
receiveStatement = makeTerm <$> symbol ReceiveStatement <*> children (Go.Syntax.Receive <$> (expression <|> emptyTerm) <*> expression)

shortVarDeclaration :: Assignment
shortVarDeclaration = makeTerm <$> symbol ShortVarDeclaration <*> children (Statement.Assignment <$> pure [] <*> expression <*> expression)

selectStatement :: Assignment
selectStatement = makeTerm <$> symbol SelectStatement <*> children (Go.Syntax.Select <$> expressions)

sendStatement :: Assignment
sendStatement = makeTerm <$> symbol SendStatement <*> children (Go.Syntax.Send <$> expression <*> expression)


-- Helpers

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Union Syntax Term)]
          -> Assignment.Assignment [] Grammar (Union Syntax Term)
infixTerm = infixContext comment

-- | Match a series of terms or comments until a delimiter is matched
manyTermsTill :: Show b
              => Assignment.Assignment [] Grammar Term
              -> Assignment.Assignment [] Grammar b
              -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm = many . term

-- | Match a term and contextualize any comments preceeding or proceeding the term.
term :: Assignment -> Assignment
term term' = contextualize comment term' <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)
