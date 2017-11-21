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
import Data.Syntax (contextualize, emptyTerm, parseError, handleError, infixContext, makeTerm, makeTerm', makeTerm1)
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
   , Go.Syntax.Composite
   , Go.Syntax.DefaultPattern
   , Go.Syntax.Defer
   , Go.Syntax.Field
   , Go.Syntax.Go
   , Go.Syntax.Label
   , Go.Syntax.Receive
   , Go.Syntax.Rune
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
   , Type.Parenthesized
   , Type.Pointer
   , Type.ReceiveChannel
   , Type.SendChannel
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
  , labelName
  , labeledStatement
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
floatLiteral = makeTerm <$> symbol FloatLiteral <*> (Literal.Float <$> source)

identifier :: Assignment
identifier =  makeTerm <$> symbol Identifier  <*> (Syntax.Identifier <$> source)
          <|> makeTerm <$> symbol Identifier' <*> (Syntax.Identifier <$> source)

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
arrayType = makeTerm <$> symbol ArrayType <*> children (Type.Array . Just <$> expression <*> (expression <|> arrayType))

channelType :: Assignment
channelType =  (makeTerm <$> symbol ChannelType <*> children (token AnonLAngleMinus *> token AnonChan *> (Type.ReceiveChannel <$> expression)))
           <|> (makeTerm <$> symbol ChannelType <*> children (token AnonChan *> token AnonLAngleMinus *> (Type.SendChannel <$> expression)))
           <|> (makeTerm <$> symbol ChannelType <*> children (token AnonChan *> (Type.BiDirectionalChannel <$> expression)))

fieldDeclaration :: Assignment
fieldDeclaration =  mkFieldDeclarationWithTag <$> symbol FieldDeclaration <*> children ((,,) <$> (manyTermsTill expression (void (symbol TypeIdentifier)) <|> (manyTerm expression)) <*> optional expression <*> optional expression)
  where
    mkFieldDeclarationWithTag loc (fields, (Just type'), (Just tag)) = makeTerm loc $ Go.Syntax.Field [type', tag] (makeTerm loc fields) --Type.Annotation (makeTerm loc (Type.Annotation (makeTerm loc fields) type')) tag
    mkFieldDeclarationWithTag loc (fields, (Just type'), Nothing) = makeTerm loc $ Go.Syntax.Field [type'] (makeTerm loc fields)
    mkFieldDeclarationWithTag loc (fields, Nothing, (Just tag)) = makeTerm loc $ Go.Syntax.Field [tag] (makeTerm loc fields)
    mkFieldDeclarationWithTag loc (fields, Nothing, Nothing) = makeTerm loc $ Go.Syntax.Field [] (makeTerm loc fields)

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


-- Type Declarations

arrayTypeDeclaration :: Assignment
arrayTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> arrayType)

channelTypeDeclaration :: Assignment
channelTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> channelType)

functionTypeDeclaration :: Assignment
functionTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> functionType)

interfaceTypeDeclaration :: Assignment
interfaceTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> interfaceType)

mapTypeDeclaration :: Assignment
mapTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> mapType)

pointerTypeDeclaration :: Assignment
pointerTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> pointerType)

qualifiedTypeDeclaration :: Assignment
qualifiedTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> qualifiedType)

sliceTypeDeclaration :: Assignment
sliceTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> sliceType)

structTypeDeclaration :: Assignment
structTypeDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> structType)

typeAlias :: Assignment
typeAlias = makeTerm <$> symbol TypeAlias <*> children (Type.Alias <$> expression <*> expression)

typeIdentifierDeclaration :: Assignment
typeIdentifierDeclaration = makeTerm <$> symbol TypeSpec <*> children (Type.Annotation <$> typeIdentifier <*> expression)

typeDeclaration :: Assignment
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (manyTerm (  arrayTypeDeclaration
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
functionDeclaration =  mkTypedFunctionDeclaration <$> symbol FunctionDeclaration <*> children ((,,,) <$> expression <*> manyTerm parameters <*> optional (types <|> identifier <|> returnParameters) <*> optional block)
                   <|> mkTypedFunctionLiteral     <$> symbol FuncLiteral         <*> children ((,,,) <$> emptyTerm  <*> manyTerm parameters <*> optional (types <|> identifier <|> returnParameters) <*> block)
  where
    mkTypedFunctionDeclaration loc (name', params', types', block') = makeTerm loc (Declaration.Function [(maybe (makeTerm loc Syntax.Empty) id types')] name' params' (maybe (makeTerm loc Syntax.Empty) id block'))
    mkTypedFunctionLiteral     loc (name', params', types', block') = makeTerm loc (Declaration.Function [(maybe (makeTerm loc Syntax.Empty) id types')] name' params' block')
    returnParameters = makeTerm <$> symbol Parameters <*> children (manyTerm expression)

importDeclaration :: Assignment
importDeclaration = makeTerm <$> symbol ImportDeclaration <*> children (Declaration.Import <$> manyTerm expression)

importSpec :: Assignment
importSpec = symbol ImportSpec *> children expressions

indexExpression :: Assignment
indexExpression = makeTerm <$> symbol IndexExpression <*> children (Expression.Subscript <$> expression <*> manyTerm expression)

methodDeclaration :: Assignment
methodDeclaration = mkTypedMethodDeclaration <$> symbol MethodDeclaration <*> children ((,,,,) <$> receiver <*> fieldIdentifier <*> manyTerm parameters <*> ((makeTerm <$> location <*> (manyTermsTill expression (void (symbol Block)))) <|> emptyTerm) <*> (block <|> emptyTerm))
  where
    receiver = symbol Parameters *> children ((symbol ParameterDeclaration *> children expressions) <|> expressions)
    mkTypedMethodDeclaration loc (receiver', name', parameters', type'', body') = makeTerm loc (Declaration.Method [type''] receiver' name' parameters' body')

methodSpec :: Assignment
methodSpec =  mkMethodSpec <$> symbol MethodSpec <*> children ((,,,,) <$> empty <*> expression <*> parameters <*> (expression <|> parameters <|> emptyTerm) <*> empty)
  where
    empty = makeTerm <$> location <*> pure Syntax.Empty
    mkMethodSpec loc (receiver', name', params, optionaltypeLiteral, body') = makeTerm loc $ Type.Annotation (mkMethod loc receiver' name' params body') optionaltypeLiteral
    mkMethod loc empty' name' params empty'' = makeTerm loc $ Declaration.Method [] empty' name' (pure params) empty''

packageClause :: Assignment
packageClause = makeTerm <$> symbol PackageClause <*> children (Declaration.Module <$> expression <*> pure [])

parameters :: Assignment
parameters = symbol Parameters *> children expressions

parameterDeclaration :: Assignment
parameterDeclaration = makeTerm <$> symbol ParameterDeclaration <*> children (manyTerm expression)

parenthesizedExpression :: Assignment
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

selectorExpression :: Assignment
selectorExpression = makeTerm <$> symbol SelectorExpression <*> children (Expression.MemberAccess <$> expression <*> expression)

sliceExpression :: Assignment
sliceExpression = makeTerm <$> symbol SliceExpression <*> children (  (Go.Syntax.Slice <$> expression <*> expression <*> expression <*> expression)
                                                                  <|> (Go.Syntax.Slice <$> expression <*> emptyTerm <* symbol AnonColon <*> expression <* symbol AnonColon <*> expression)
                                                                  <|> (Go.Syntax.Slice <$> expression <*> emptyTerm <* symbol AnonColon <*> expression <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> expression <*> expression <*> expression <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> expression <*> expression <*> emptyTerm <*> emptyTerm)
                                                                  <|> (Go.Syntax.Slice <$> expression <*> emptyTerm <*> emptyTerm <*> emptyTerm))

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
unaryExpression = symbol UnaryExpression >>= \ location -> (notExpression location) <|> (unaryMinus location) <|> unaryPlus <|> unaryAmpersand <|> unaryReceive <|> unaryPointer <|> unaryComplement
  where
    notExpression location = makeTerm location . Expression.Not <$> children (symbol AnonBang *> expression)
    unaryMinus location    = makeTerm location . Expression.Negate <$> children (symbol AnonMinus *> expression)
    unaryPlus = children (symbol AnonPlus *> expression)
    unaryAmpersand = children (makeTerm <$> symbol AnonAmpersand <*> (Literal.Reference <$> expression))
    unaryReceive = children (makeTerm <$> symbol AnonLAngleMinus <*> (Go.Syntax.Receive <$> emptyTerm <*> expression))
    unaryPointer = children (makeTerm <$> symbol AnonStar <*> (Literal.Pointer <$> expression))
    unaryComplement = children (makeTerm <$> symbol AnonCaret <*> (Expression.Complement <$> expression))

varDeclaration :: Assignment
varDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

variadicArgument :: Assignment
variadicArgument = makeTerm <$> symbol VariadicArgument <*> children (Go.Syntax.Variadic <$> pure [] <*> expression)

variadicParameterDeclaration :: Assignment
variadicParameterDeclaration =  mkVariadic <$> symbol VariadicParameterDeclaration <*> children ((,) <$> emptyTerm  <*> expression)
                            <|> mkVariadic <$> symbol VariadicParameterDeclaration <*> children ((,) <$> expression <*> expression)
  where
    mkVariadic loc (identifier', typeIdentifier') = makeTerm loc (Go.Syntax.Variadic [typeIdentifier'] identifier')

varSpecification :: Assignment
varSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment <$> pure [] <*> (annotatedLHS <|> identifiers) <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation <$> (makeTerm <$> location <*> (manyTermsTill identifier (void (symbol TypeIdentifier)))) <*> expression)


-- Statements

assignment' :: Assignment
assignment' =  makeTerm'  <$> symbol AssignmentStatement <*> children (infixTerm expressionList expressionList
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
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (labelName <|> emptyTerm))

communicationClause :: Assignment
communicationClause = makeTerm <$> symbol CommunicationClause <*> children (Statement.Pattern <$> (communicationCase <|> defaultCase) <*> expressions)
  where
    communicationCase = symbol CommunicationCase *> children expression

continueStatement :: Assignment
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (labelName <|> emptyTerm))

decStatement :: Assignment
decStatement = makeTerm <$> symbol DecStatement <*> children (Statement.PostDecrement <$> expression)

deferStatement :: Assignment
deferStatement = makeTerm <$> symbol DeferStatement <*> children (Go.Syntax.Defer <$> expression)

elseClause :: Assignment
elseClause = symbol ElseClause *> children expression

emptyStatement :: Assignment
emptyStatement = makeTerm <$> token EmptyStatement <*> (Statement.NoOp <$> emptyTerm)

forStatement :: Assignment
forStatement = mkForStatement <$> symbol ForStatement <*> children ((,) <$> (forClause <|> rangeClause <|> for <|> emptyClause) <*> expression)
  where
    mkForStatement loc ((constructor, a, b, c), block') = case (constructor :: [Char]) of
                                                            "forEach" -> makeTerm loc $ (Statement.ForEach a b block')
                                                            _ -> makeTerm loc $ (Statement.For a b c block')
    emptyClause = children (("for",,,) <$> emptyTerm <*> emptyTerm <*> emptyTerm)
    rangeClause = symbol RangeClause *> children ( (("forEach",,,) <$> expression <*> expression <*> emptyTerm)
                                                <|> (("forEach",,,) <$> emptyTerm <*> expression <*> emptyTerm))
    forClause = symbol ForClause *> children (  (("for",,,) <$> expression <*> expression <*> expression)
                                            <|> (("for",,,) <$> expression <*> expression <*> emptyTerm)
                                            <|> (("for",,,) <$> expression <*> emptyTerm <*> emptyTerm)
                                            <|> (("for",,,) <$> emptyTerm <*> emptyTerm <*> emptyTerm))
    for = ("for",,,) <$> emptyTerm <*> expression <*> emptyTerm

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
receiveStatement = makeTerm <$> symbol ReceiveStatement <*> children (  (Go.Syntax.Receive <$> expression <*> expression)
                                                                    <|> (Go.Syntax.Receive <$> emptyTerm <*> expression))

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
