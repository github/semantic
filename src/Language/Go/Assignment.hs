{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TupleSections, TypeOperators #-}
module Language.Go.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Data.Functor (void)
import Data.List.NonEmpty (some1)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, parseError, handleError, infixContext, makeTerm, makeTerm', makeTerm1, postContextualize)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import Data.Union
import GHC.Stack
import Language.Go.Grammar as Grammar
import Language.Go.Syntax as Go.Syntax

type Syntax =
  '[ Comment.Comment
   , Declaration.Constructor
   , Declaration.Function
   , Declaration.Import
   , Declaration.Method
   , Declaration.Module
   , Expression.Arithmetic
   , Expression.Bitwise
   , Expression.Boolean
   , Expression.Call
   , Expression.Comparison
   , Expression.Subscript
   , Statement.PostDecrement
   , Statement.PostIncrement
   , Expression.MemberAccess
   , Go.Syntax.DefaultPattern
   , Go.Syntax.Defer
   , Go.Syntax.Field
   , Go.Syntax.Go
   , Go.Syntax.Label
   , Go.Syntax.ParenthesizedType
   , Go.Syntax.Receive
   , Go.Syntax.RuneLiteral
   , Go.Syntax.Select
   , Go.Syntax.Send
   , Go.Syntax.Slice
   , Go.Syntax.TypeAssertion
   , Go.Syntax.TypeConversion
   , Go.Syntax.TypeSwitch
   , Go.Syntax.TypeSwitchGuard
   , Go.Syntax.Variadic
   , Literal.Array
   , Literal.Channel
   , Literal.Complex
   , Literal.Composite
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
   , Type.Alias
   , Type.Annotation
   , Type.Array
   , Type.BiDirectionalChannel
   , Type.Function
   , Type.Interface
   , Type.Map
   , Type.Pointer
   , Type.ReceiveChannel
   , Type.SendChannel
   , Type.Slice
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- Helpers

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment
          -> Assignment
          -> [Assignment.Assignment [] Grammar (Term -> Term -> Union Syntax Term)]
          -> Assignment.Assignment [] Grammar (Union Syntax Term)
infixTerm = infixContext comment

-- | Match a series of terms or comments until a delimiter is matched
manyTermsTill :: Show b => Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm term = many (contextualize comment term <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm))

-- | Match a term and contextualize any comments preceeding or proceeding the term.
term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)


-- | Assignment from AST in Go's grammar onto a program in Go's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> manyTerm expression) <|> parseError

expression :: Assignment
expression = handleError (choice expressionChoices)

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices =
  [ assignment'
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
  , importSpec
  , indexExpression
  , interpretedStringLiteral
  , intLiteral
  , keyedElement
  , labelName'
  , labelStatement'
  , literalValue
  , methodDeclaration
  , methodSpec
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

identifiers :: Assignment
identifiers = mk <$> location <*> manyTerm identifier
  where
    mk _ [a] = a
    mk loc children = makeTerm loc children

expressions :: Assignment
expressions = mk <$> location <*> manyTerm expression
  where
    mk _ [a] = a
    mk loc children = makeTerm loc children

types :: Assignment
types =  arrayType
     <|> channelType
     <|> functionType
     <|> implicitLengthArrayType
     <|> interfaceType
     <|> mapType
     <|> parenthesizedType
     <|> pointerType
     <|> qualifiedType
     <|> sliceType
     <|> structType
     <|> typeAssertion
     <|> typeConversion
     <|> typeDeclaration
     <|> typeIdentifier
     <|> typeCase
     <|> typeCaseClause
     <|> typeSwitchGuard
     <|> typeSwitchStatement

program :: Assignment
program = makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> many expression)

-- Literals

element :: Assignment
element = symbol Element *> children expression

imaginaryLiteral :: Assignment
imaginaryLiteral = makeTerm <$> symbol ImaginaryLiteral <*> (Literal.Complex <$> source)

literalValue :: Assignment
literalValue = makeTerm <$> symbol LiteralValue <*> children (manyTerm expression)

compositeLiteral :: Assignment
compositeLiteral = makeTerm <$> symbol CompositeLiteral <*> children (Literal.Composite <$> term expression <*> term expression)

intLiteral :: Assignment
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

floatLiteral :: Assignment
floatLiteral = makeTerm <$> symbol FloatLiteral <*> (Literal.Float <$> source)

rawStringLiteral :: Assignment
rawStringLiteral = makeTerm <$> symbol RawStringLiteral <*> (Literal.TextElement <$> source)

typeIdentifier :: Assignment
typeIdentifier = makeTerm <$> symbol TypeIdentifier <*> (Syntax.Identifier <$> source)

identifier :: Assignment
identifier = makeTerm <$> (symbol Identifier <|> symbol Identifier') <*> (Syntax.Identifier <$> source)

fieldIdentifier :: Assignment
fieldIdentifier = makeTerm <$> symbol FieldIdentifier <*> (Syntax.Identifier <$> source)

packageIdentifier :: Assignment
packageIdentifier = makeTerm <$> symbol PackageIdentifier <*> (Syntax.Identifier <$> source)

parenthesizedType :: Assignment
parenthesizedType = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Go.Syntax.ParenthesizedType <$> term expression)

interpretedStringLiteral :: Assignment
interpretedStringLiteral = makeTerm <$> symbol InterpretedStringLiteral <*> (Literal.TextElement <$> source)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

runeLiteral :: Assignment
runeLiteral = makeTerm <$> symbol Grammar.RuneLiteral <*> (Go.Syntax.RuneLiteral <$> source)


-- Primitive Types

qualifiedType :: Assignment
qualifiedType = makeTerm <$> symbol QualifiedType <*> children (Expression.MemberAccess <$> term expression <*> term expression)

arrayType :: Assignment
arrayType = makeTerm <$> symbol ArrayType <*> children (Type.Array . Just <$> term expression <*> (term expression <|> term arrayType))

implicitLengthArrayType :: Assignment
implicitLengthArrayType = makeTerm <$> symbol ImplicitLengthArrayType <*> children (Type.Array Nothing <$> term expression)

functionType :: Assignment
functionType = makeTerm <$> symbol FunctionType <*> children (Type.Function <$> manyTerm parameters <*> term returnType)
  where
    returnType = symbol Parameters *> children expressions <|> term expression <|> emptyTerm

sliceType :: Assignment
sliceType = makeTerm <$> symbol SliceType <*> children (Type.Slice <$> term expression)

channelType :: Assignment
channelType =  (makeTerm <$> symbol ChannelType <*> children (token AnonLAngleMinus *> token AnonChan *> (Type.ReceiveChannel <$> term expression)))
           <|> (makeTerm <$> symbol ChannelType <*> children (token AnonChan *> token AnonLAngleMinus *> (Type.SendChannel <$> term expression)))
           <|> (makeTerm <$> symbol ChannelType <*> children (token AnonChan *> (Type.BiDirectionalChannel <$> term expression)))

structType :: Assignment
structType = handleError $ makeTerm <$> symbol StructType <*> children (Declaration.Constructor <$> emptyTerm <*> manyTerm expression)

interfaceType :: Assignment
interfaceType = handleError $ makeTerm <$> symbol InterfaceType <*> children (Type.Interface <$> manyTerm expression)

mapType :: Assignment
mapType = handleError $ makeTerm <$> symbol MapType <*> children (Type.Map <$> term expression <*> term expression)

pointerType :: Assignment
pointerType = handleError $ makeTerm <$> symbol PointerType <*> children (Type.Pointer <$> term expression)

fieldDeclaration :: Assignment
fieldDeclaration =  mkFieldDeclarationWithTag <$> symbol FieldDeclaration <*> children ((,,) <$> (manyTermsTill expression (void (symbol TypeIdentifier)) <|> (manyTerm expression)) <*> optional (term expression) <*> optional (term expression))
  where
    mkFieldDeclarationWithTag loc (fields, (Just type'), (Just tag)) = makeTerm loc $ Go.Syntax.Field [type', tag] (makeTerm loc fields) --Type.Annotation (makeTerm loc (Type.Annotation (makeTerm loc fields) type')) tag
    mkFieldDeclarationWithTag loc (fields, (Just type'), Nothing) = makeTerm loc $ Go.Syntax.Field [type'] (makeTerm loc fields)
    mkFieldDeclarationWithTag loc (fields, Nothing, (Just tag)) = makeTerm loc $ Go.Syntax.Field [tag] (makeTerm loc fields)
    mkFieldDeclarationWithTag loc (fields, Nothing, Nothing) = makeTerm loc $ Go.Syntax.Field [] (makeTerm loc fields)


-- Type Declarations

channelTypeDeclaration :: Assignment
channelTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term channelType)

functionTypeDeclaration :: Assignment
functionTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term functionType)

interfaceTypeDeclaration :: Assignment
interfaceTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term interfaceType)

mapTypeDeclaration :: Assignment
mapTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term mapType)

structTypeDeclaration :: Assignment
structTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term structType)

qualifiedTypeDeclaration :: Assignment
qualifiedTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term qualifiedType)

arrayTypeDeclaration :: Assignment
arrayTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term arrayType)

sliceTypeDeclaration :: Assignment
sliceTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term sliceType)

pointerTypeDeclaration :: Assignment
pointerTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term pointerType)

typeAlias :: Assignment
typeAlias = makeTerm <$> symbol TypeAlias <*> children (Type.Alias <$> term expression <*> term expression)

typeIdentifierDeclaration :: Assignment
typeIdentifierDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> term typeIdentifier <*> term expression)

typeDeclaration :: Assignment
typeDeclaration = handleError $ makeTerm <$> symbol TypeDeclaration <*> children (manyTerm (  arrayTypeDeclaration
                                                                                          <|> channelTypeDeclaration
                                                                                          <|> functionTypeDeclaration
                                                                                          <|> interfaceTypeDeclaration
                                                                                          <|> qualifiedTypeDeclaration
                                                                                          <|> pointerTypeDeclaration
                                                                                          <|> sliceTypeDeclaration
                                                                                          <|> structTypeDeclaration
                                                                                          <|> mapTypeDeclaration
                                                                                          <|> typeAlias
                                                                                          <|> typeIdentifierDeclaration ))


-- Expressions

indexExpression :: Assignment
indexExpression = makeTerm <$> symbol IndexExpression <*> children (Expression.Subscript <$> term expression <*> manyTerm expression)

sliceExpression :: Assignment
sliceExpression = makeTerm <$> symbol SliceExpression <*> children (  (Go.Syntax.Slice <$> term expression <*> term expression <*> term expression <*> term expression)
                                                                  <|> (Go.Syntax.Slice <$> term expression <*> emptyTerm <* symbol AnonColon <*> term expression <* symbol AnonColon <*> term expression)
                                                                  <|> (Go.Syntax.Slice <$> term expression <*> emptyTerm <* symbol AnonColon <*> term expression <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> term expression <*> term expression <*> term expression <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> term expression <*> term expression <*> emptyTerm <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> term expression <*> emptyTerm <*> emptyTerm <*> emptyTerm))

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

selectorExpression :: Assignment
selectorExpression = makeTerm <$> symbol SelectorExpression <*> children (Expression.MemberAccess <$> term expression <*> term expression)

typeAssertion :: Assignment
typeAssertion = makeTerm <$> symbol TypeAssertionExpression <*> children (Go.Syntax.TypeAssertion <$> term expression <*> term expression)

typeConversion :: Assignment
typeConversion = makeTerm <$> symbol TypeConversionExpression <*> children (Go.Syntax.TypeConversion <$> term expression <*> term expression)

unaryExpression :: Assignment
unaryExpression = symbol UnaryExpression >>= \ location -> (notExpression location) <|> (unaryMinus location) <|> unaryPlus <|> unaryAmpersand <|> unaryReceive <|> unaryPointer <|> unaryComplement
  where
    notExpression location = makeTerm location . Expression.Not <$> children (symbol AnonBang *> term expression)
    unaryMinus location    = makeTerm location . Expression.Negate <$> children (symbol AnonMinus *> term expression)
    unaryPlus = children (symbol AnonPlus *> expression)
    unaryAmpersand = children (makeTerm <$> symbol AnonAmpersand <*> (Literal.Reference <$> term expression))
    unaryReceive = children (makeTerm <$> symbol AnonLAngleMinus <*> (Go.Syntax.Receive <$> emptyTerm <*> term expression))
    unaryPointer = children (makeTerm <$> symbol AnonStar <*> (Literal.Pointer <$> term expression))
    unaryComplement = children (makeTerm <$> symbol AnonCaret <*> (Expression.Complement <$> term expression))

binaryExpression :: Assignment
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression (contextualize comment expression)
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

expressionCase :: Assignment
expressionCase = makeTerm <$> symbol ExpressionCase <*> (Statement.Pattern <$> children expressions <*> expressions)

defaultCase :: Assignment
defaultCase = makeTerm <$> symbol DefaultCase <*> children (Go.Syntax.DefaultPattern <$> (expressions <|> emptyTerm))

defaultExpressionCase :: Assignment
defaultExpressionCase = makeTerm <$> symbol DefaultCase <*> (Go.Syntax.DefaultPattern <$ source <*> (expressions <|> emptyTerm))

expressionCaseClause :: Assignment
expressionCaseClause = symbol ExpressionCaseClause *> children (term (expressionCase <|> defaultExpressionCase))

expressionSwitchStatement :: Assignment
expressionSwitchStatement = makeTerm <$> symbol ExpressionSwitchStatement <*> children (Statement.Match <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ExpressionCaseClause)) <|> emptyTerm) <*> expressions)

typeSwitchStatement :: Assignment
typeSwitchStatement = makeTerm <$> symbol TypeSwitchStatement <*> children (Go.Syntax.TypeSwitch <$> term _typeSwitchSubject <*> expressions)
  where
    _typeSwitchSubject = makeTerm <$> location <*> manyTermsTill expression (void (symbol TypeCaseClause))

typeSwitchGuard :: Assignment
typeSwitchGuard = makeTerm <$> symbol Grammar.TypeSwitchGuard <*> children (Go.Syntax.TypeSwitchGuard <$> expressions)

typeCaseClause :: Assignment
typeCaseClause = makeTerm <$> symbol TypeCaseClause <*> children (Statement.Pattern <$> term expression <*> expressions)

typeCase :: Assignment
typeCase = symbol TypeCase *> children expressions

fallThroughStatement :: Assignment
fallThroughStatement = makeTerm <$> symbol FallthroughStatement <*> (Statement.Pattern <$> (makeTerm <$> location <*> (Syntax.Identifier <$> source)) <*> emptyTerm)

variadicArgument :: Assignment
variadicArgument = makeTerm <$> symbol VariadicArgument <*> children (Go.Syntax.Variadic <$> pure [] <*> term expression)

callExpression :: Assignment
callExpression = makeTerm <$> symbol CallExpression <*> children (Expression.Call <$> pure [] <*> term expression <*> manyTerm expression <*> emptyTerm)

varDeclaration :: Assignment
varDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

varSpecification :: Assignment
varSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment <$> pure [] <*> (annotatedLHS <|> identifiers) <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation <$> (makeTerm <$> location <*> (manyTermsTill identifier (void (symbol TypeIdentifier)))) <*> term expression)

expressionList :: Assignment
expressionList = symbol ExpressionList *> children expressions

functionDeclaration :: Assignment
functionDeclaration =  mkTypedFunctionDeclaration <$> symbol FunctionDeclaration <*> children ((,,,) <$> term expression <*> manyTerm parameters <*> optional (term types <|> term identifier <|> term returnParameters) <*> optional (term block))
                   <|> mkTypedFunctionLiteral     <$> symbol FuncLiteral         <*> children ((,,,) <$> emptyTerm  <*> manyTerm parameters <*> optional (term types <|> term identifier <|> term returnParameters) <*> term block)
  where
    mkTypedFunctionDeclaration loc (name', params', types', block') = makeTerm loc (Declaration.Function [(maybe (makeTerm loc Syntax.Empty) id types')] name' params' (maybe (makeTerm loc Syntax.Empty) id block'))
    mkTypedFunctionLiteral     loc (name', params', types', block') = makeTerm loc (Declaration.Function [(maybe (makeTerm loc Syntax.Empty) id types')] name' params' block')
    returnParameters = makeTerm <$> symbol Parameters <*> children (manyTerm expression)

variadicParameterDeclaration :: Assignment
variadicParameterDeclaration =  mkVariadic <$> symbol VariadicParameterDeclaration <*> children ((,) <$> emptyTerm  <*> term expression)
                            <|> mkVariadic <$> symbol VariadicParameterDeclaration <*> children ((,) <$> term expression <*> term expression)
  where
    mkVariadic loc (identifier', typeIdentifier') = makeTerm loc (Go.Syntax.Variadic [typeIdentifier'] identifier')

importDeclaration :: Assignment
importDeclaration = makeTerm <$> symbol ImportDeclaration <*> children (Declaration.Import <$> manyTerm expression)

importSpec :: Assignment
importSpec = symbol ImportSpec *> children expressions

parameters :: Assignment
parameters = makeTerm <$> symbol Parameters <*> children (manyTerm expression)

parameterDeclaration :: Assignment
parameterDeclaration = makeTerm <$> symbol ParameterDeclaration <*> children (manyTerm expression)

methodDeclaration :: Assignment
methodDeclaration = mkTypedMethodDeclaration <$> symbol MethodDeclaration <*> children ((,,,,) <$> term receiver <*> term fieldIdentifier <*> manyTerm parameters <*> term ((makeTerm <$> location <*> (manyTermsTill expression (void (symbol Block)))) <|> emptyTerm) <*> term (block <|> emptyTerm))
  where
    receiver = symbol Parameters *> children ((symbol ParameterDeclaration *> children expressions) <|> expressions)
    mkTypedMethodDeclaration loc (receiver', name', parameters', type'', body') = makeTerm loc (Declaration.Method [type''] receiver' name' parameters' body')

methodSpec :: Assignment
methodSpec =  mkMethodSpec <$> symbol MethodSpec <*> children ((,,,,) <$> empty <*> term expression <*> term parameters <*> (term expression <|> term parameters <|> term emptyTerm) <*> term empty)
  where
    empty = makeTerm <$> location <*> pure Syntax.Empty
    mkMethodSpec loc (receiver', name', params, optionaltypeLiteral, body') = makeTerm loc $ Type.Annotation (mkMethod loc receiver' name' params body') optionaltypeLiteral
    mkMethod loc empty' name' params empty'' = makeTerm loc $ Declaration.Method [] empty' name' (pure params) empty''

packageClause :: Assignment
packageClause = makeTerm <$> symbol PackageClause <*> children (Declaration.Module <$> term expression <*> pure [])


-- Statements

assignment' :: Assignment
assignment' =  makeTerm'  <$> symbol AssignmentStatement <*> children (infixTerm expressionList (term expressionList)
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

emptyStatement :: Assignment
emptyStatement = makeTerm <$> token EmptyStatement <*> (Statement.NoOp <$> emptyTerm)

shortVarDeclaration :: Assignment
shortVarDeclaration = makeTerm <$> symbol ShortVarDeclaration <*> children (Statement.Assignment <$> pure [] <*> term expression <*> term expression)

sendStatement :: Assignment
sendStatement = makeTerm <$> symbol SendStatement <*> children (Go.Syntax.Send <$> term expression <*> term expression)

breakStatement :: Assignment
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (term labelName' <|> term emptyTerm))

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (term labelName' <|> term emptyTerm))

decStatement :: Assignment
decStatement = makeTerm <$> symbol DecStatement <*> children (Statement.PostDecrement <$> term expression)

deferStatement :: Assignment
deferStatement = makeTerm <$> symbol DeferStatement <*> children (Go.Syntax.Defer <$> term expression)

goStatement :: Assignment
goStatement = makeTerm <$> symbol GoStatement <*> children (Go.Syntax.Go <$> term expression)

gotoStatement :: Assignment
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> term expression)

ifStatement :: Assignment
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol Block))) <*> term expression <*> (term expression <|> term emptyTerm))

ifInitializer :: Assignment
ifInitializer = symbol IfInitializer *> children (term expression)

elseClause :: Assignment
elseClause = symbol ElseClause *> children (term expression)

forStatement :: Assignment
forStatement = mkForStatement <$> symbol ForStatement <*> children ((,) <$> (forClause <|> rangeClause <|> for <|> emptyClause) <*> term expression)
  where
    mkForStatement loc ((constructor, a, b, c), block') = case (constructor :: [Char]) of
                                                            "forEach" -> makeTerm loc $ (Statement.ForEach a b block')
                                                            _ -> makeTerm loc $ (Statement.For a b c block')
    emptyClause = children (("for",,,) <$> emptyTerm <*> emptyTerm <*> emptyTerm)
    rangeClause = symbol RangeClause *> children ( (("forEach",,,) <$> term expression <*> term expression <*> emptyTerm)
                                                <|> (("forEach",,,) <$> term emptyTerm <*> term expression <*> emptyTerm))
    forClause = symbol ForClause *> children (  (("for",,,) <$> term expression <*> term expression <*> term expression)
                                            <|> (("for",,,) <$> term expression <*> term expression <*> emptyTerm)
                                            <|> (("for",,,) <$> term expression <*> emptyTerm <*> emptyTerm)
                                            <|> (("for",,,) <$> term emptyTerm <*> emptyTerm <*> emptyTerm))
    for = ("for",,,) <$> emptyTerm <*> term expression <*> emptyTerm

incStatement :: Assignment
incStatement = makeTerm <$> symbol IncStatement <*> children (Statement.PostIncrement <$> term expression)

keyedElement :: Assignment
keyedElement = makeTerm <$> symbol KeyedElement <*> children (Literal.KeyValue <$> term expression <*> term expression)

labelName' :: Assignment
labelName' = makeTerm <$> symbol LabelName <*> (Syntax.Identifier <$> source)

labelStatement' :: Assignment
labelStatement' = makeTerm <$> symbol LabelStatement <*> children (Go.Syntax.Label <$> term expression <*> (term expression <|> emptyTerm))

returnStatement :: Assignment
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (term expression <|> emptyTerm))

receiveStatement :: Assignment
receiveStatement = makeTerm <$> symbol ReceiveStatement <*> children (  (Go.Syntax.Receive <$> term expression <*> term expression)
                                                                    <|> (Go.Syntax.Receive <$> emptyTerm <*> term expression))

selectStatement :: Assignment
selectStatement = makeTerm <$> symbol SelectStatement <*> children (Go.Syntax.Select <$> expressions)

communicationClause :: Assignment
communicationClause = makeTerm <$> symbol CommunicationClause <*> children (Statement.Pattern <$> (term communicationCase <|> term defaultCase) <*> expressions)
  where
    communicationCase = symbol CommunicationCase *> children expression
