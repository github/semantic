{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Go.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Prologue

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import           Data.Abstract.Name (Name, name)
import           Data.Record
import           Data.Syntax
    (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm'', makeTerm1, parseError)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import qualified Data.Diff as Diff
import           Language.Go.Grammar as Grammar
import           Language.Go.Syntax as Go.Syntax hiding (runeLiteral, labelName)
import           Language.Go.Type as Go.Type
import           Proto3.Suite (Named (..), Named1 (..))

type Syntax =
  '[ Comment.Comment
   , Declaration.Constructor
   , Declaration.Function
   , Declaration.Method
   , Declaration.MethodSignature
   , Declaration.Type
   , Declaration.TypeAlias
   , Expression.Plus
   , Expression.Minus
   , Expression.Times
   , Expression.DividedBy
   , Expression.Modulo
   , Expression.Power
   , Expression.Negate
   , Expression.FloorDivision
   , Expression.BOr
   , Expression.BAnd
   , Expression.BXOr
   , Expression.LShift
   , Expression.RShift
   , Expression.UnsignedRShift
   , Expression.Complement
   , Expression.Call
   , Expression.LessThan
   , Expression.LessThanEqual
   , Expression.GreaterThan
   , Expression.GreaterThanEqual
   , Expression.Equal
   , Expression.StrictEqual
   , Expression.Comparison
   , Expression.Subscript
   , Expression.Member
   , Statement.PostDecrement
   , Statement.PostIncrement
   , Expression.MemberAccess
   , Expression.And
   , Expression.Not
   , Expression.Or
   , Expression.XOr
   , Go.Syntax.Composite
   , Go.Syntax.DefaultPattern
   , Go.Syntax.Defer
   , Go.Syntax.Field
   , Go.Syntax.Go
   , Go.Syntax.Label
   , Go.Syntax.Package
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
   , Go.Syntax.Import
   , Go.Syntax.QualifiedImport
   , Go.Syntax.SideEffectImport
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
   , Statement.Statements
   , Syntax.Context
   , Syntax.Error
   , Syntax.Empty
   , Syntax.Identifier
   , Type.Annotation
   , Type.Array
   , Type.Function
   , Type.Interface
   , Type.Map
   , Type.Parenthesized
   , Type.Pointer
   , Type.Slice
   , []
   , Literal.String
   , Literal.EscapeSequence
   ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar

-- For Protobuf serialization
instance Named1 (Sum Syntax) where nameOf1 _ = "GoSyntax"
instance Named (Term.Term (Sum Syntax) ()) where nameOf _ = "GoTerm"
instance Named (Diff.Diff (Sum Syntax) () ()) where nameOf _ = "GoDiff"

-- | Assignment from AST in Go's grammar onto a program in Go's syntax.
assignment :: Assignment Term
assignment = handleError program <|> parseError

program :: Assignment Term
program = makeTerm <$> symbol SourceFile <*> children (Statement.Statements <$> manyTerm expression)

expression :: Assignment Term
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment Term]
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

types :: Assignment Term
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

identifiers :: Assignment Term
identifiers = makeTerm'' <$> location <*> manyTerm identifier

expressions :: Assignment Term
expressions = makeTerm'' <$> location <*> manyTerm expression


-- Literals

comment :: Assignment Term
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

compositeLiteral :: Assignment Term
compositeLiteral = makeTerm <$> symbol CompositeLiteral <*> children (Go.Syntax.Composite <$> expression <*> expression)

element :: Assignment Term
element = symbol Element *> children expression

fieldIdentifier :: Assignment Term
fieldIdentifier = makeTerm <$> symbol FieldIdentifier <*> (Syntax.Identifier . name <$> source)

fieldIdentifier' :: Assignment Name
fieldIdentifier' = symbol FieldIdentifier *> (name <$> source)

floatLiteral :: Assignment Term
floatLiteral = makeTerm <$> symbol FloatLiteral <*> (Literal.Float <$> source)

identifier :: Assignment Term
identifier =  makeTerm <$> (symbol Identifier <|> symbol Identifier' <|> symbol Identifier'') <*> (Syntax.Identifier . name <$> source)

identifier' :: Assignment Name
identifier' =  (symbol Identifier <|> symbol Identifier' <|> symbol Identifier'') *> (name <$> source)

imaginaryLiteral :: Assignment Term
imaginaryLiteral = makeTerm <$> symbol ImaginaryLiteral <*> (Literal.Complex <$> source)

interpretedStringLiteral :: Assignment Term
interpretedStringLiteral = makeTerm' <$> symbol InterpretedStringLiteral <*> children (  (inject . Literal.String <$> some escapeSequence)
                                                                                     <|> (inject . Literal.TextElement <$> source))

escapeSequence :: Assignment Term
escapeSequence = makeTerm <$> symbol EscapeSequence <*> (Literal.EscapeSequence <$> source)

intLiteral :: Assignment Term
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

literalValue :: Assignment Term
literalValue = makeTerm <$> symbol LiteralValue <*> children (manyTerm expression)

packageIdentifier :: Assignment Term
packageIdentifier = makeTerm <$> symbol PackageIdentifier <*> (Syntax.Identifier . name <$> source)

parenthesizedType :: Assignment Term
parenthesizedType = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Type.Parenthesized <$> expression)

rawStringLiteral :: Assignment Term
rawStringLiteral = makeTerm <$> symbol RawStringLiteral <*> (Literal.TextElement <$> source)

runeLiteral :: Assignment Term
runeLiteral = makeTerm <$> symbol Grammar.RuneLiteral <*> (Go.Syntax.Rune <$> source)

typeIdentifier :: Assignment Term
typeIdentifier = makeTerm <$> symbol TypeIdentifier <*> (Syntax.Identifier . name <$> source)

typeIdentifier' :: Assignment Name
typeIdentifier' = symbol TypeIdentifier *> (name <$> source)


-- Primitive Types

arrayType :: Assignment Term
arrayType = makeTerm <$> symbol ArrayType <*> children (Type.Array . Just <$> expression <*> expression)

channelType :: Assignment Term
channelType =  makeTerm' <$> symbol ChannelType <*> children (mkChannelType <$> optional (token AnonLAngleMinus) <* token AnonChan <*> optional (token AnonLAngleMinus) <*> expression)
  where
    mkChannelType :: Maybe a -> Maybe a -> b -> Sum Syntax b
    mkChannelType receive send | Just _ <- receive = inject . Go.Type.ReceiveChannel
                               | Just _ <- send    = inject . Go.Type.SendChannel
                               | otherwise         = inject . Go.Type.BidirectionalChannel

fieldDeclaration :: Assignment Term
fieldDeclaration =  mkFieldDeclarationWithTag <$> symbol FieldDeclaration <*> children ((,,) <$> (manyTermsTill expression (void (symbol TypeIdentifier)) <|> manyTerm expression) <*> optional expression <*> optional expression)
  where
    mkFieldDeclarationWithTag loc (fields, type', tag) | Just ty <- type', Just tag' <- tag = makeTerm loc (Go.Syntax.Field [ty, tag'] (makeTerm loc fields))
                                                       | Just ty <- type'                   = makeTerm loc (Go.Syntax.Field [ty] (makeTerm loc fields))
                                                       | Just tag' <- tag                   = makeTerm loc (Go.Syntax.Field [tag'] (makeTerm loc fields))
                                                       | otherwise                          = makeTerm loc (Go.Syntax.Field [] (makeTerm loc fields))

fieldDeclarationList :: Assignment Term
fieldDeclarationList = symbol FieldDeclarationList *> children expressions

functionType :: Assignment Term
functionType = makeTerm <$> symbol FunctionType <*> children (Type.Function <$> params <*> (expression <|> emptyTerm))
  where params = symbol ParameterList *> children (manyTerm expression)

implicitLengthArrayType :: Assignment Term
implicitLengthArrayType = makeTerm <$> symbol ImplicitLengthArrayType <*> children (Type.Array Nothing <$> expression)

interfaceType :: Assignment Term
interfaceType = makeTerm <$> symbol InterfaceType <*> children (Type.Interface <$> manyTerm expression)

mapType :: Assignment Term
mapType = makeTerm <$> symbol MapType <*> children (Type.Map <$> expression <*> expression)

pointerType :: Assignment Term
pointerType = makeTerm <$> symbol PointerType <*> children (Type.Pointer <$> expression)

qualifiedType :: Assignment Term
qualifiedType = makeTerm <$> symbol QualifiedType <*> children (Expression.MemberAccess <$> expression <*> (identifier' <|> typeIdentifier'))

sliceType :: Assignment Term
sliceType = makeTerm <$> symbol SliceType <*> children (Type.Slice <$> expression)

structType :: Assignment Term
structType = makeTerm <$> symbol StructType <*> children (Declaration.Constructor [] <$> emptyTerm <*> expressions)

typeAlias :: Assignment Term
typeAlias = makeTerm <$> symbol TypeAlias <*> children (Declaration.TypeAlias [] <$> expression <*> expression)

typeDeclaration :: Assignment Term
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (manyTerm ( (makeTerm <$> symbol TypeSpec <*> children (Declaration.Type <$> typeIdentifier <*> expression))
                                                                            <|> typeAlias ))



-- Expressions

argumentList :: Assignment Term
argumentList = (symbol ArgumentList <|> symbol ArgumentList') *> children expressions

binaryExpression :: Assignment Term
binaryExpression = makeTerm' <$> symbol BinaryExpression <*> children (infixTerm expression expression
  [ (inject .) . Expression.Plus             <$ symbol AnonPlus
  , (inject .) . Expression.Minus            <$ symbol AnonMinus
  , (inject .) . Expression.Times            <$ symbol AnonStar
  , (inject .) . Expression.DividedBy        <$ symbol AnonSlash
  , (inject .) . Expression.Modulo           <$ symbol AnonPercent
  , (inject .) . Expression.Or               <$ symbol AnonPipePipe
  , (inject .) . Expression.And              <$ symbol AnonAmpersandAmpersand
  , (inject .) . Expression.LessThan         <$ symbol AnonLAngle
  , (inject .) . Expression.LessThanEqual    <$ symbol AnonLAngleEqual
  , (inject .) . Expression.GreaterThan      <$ symbol AnonRAngle
  , (inject .) . Expression.GreaterThanEqual <$ symbol AnonRAngleEqual
  , (inject .) . invert Expression.Equal     <$ symbol AnonBangEqual
  , (inject .) . Expression.Equal            <$ symbol AnonEqualEqual
  , (inject .) . Expression.BOr              <$ symbol AnonPipe
  , (inject .) . Expression.BAnd             <$ symbol AnonAmpersand
  , (inject .) . Expression.BAnd             <$ symbol AnonAmpersandCaret
  , (inject .) . Expression.BXOr             <$ symbol AnonCaret
  , (inject .) . Expression.LShift           <$ symbol AnonLAngleLAngle
  , (inject .) . Expression.RShift           <$ symbol AnonRAngleRAngle
  ])
  where
    invert cons a b = Expression.Not (makeTerm1 (cons a b))

block :: Assignment Term
block = symbol Block *> children expressions

defaultCase :: Assignment Term
defaultCase = makeTerm <$> symbol DefaultCase <*> children (Go.Syntax.DefaultPattern <$> (expressions <|> emptyTerm))

defaultExpressionCase :: Assignment Term
defaultExpressionCase = makeTerm <$> symbol DefaultCase <*> (Go.Syntax.DefaultPattern <$ rawSource <*> (expressions <|> emptyTerm))

callExpression :: Assignment Term
callExpression = makeTerm <$> symbol CallExpression <*> children (Expression.Call [] <$> expression <*> manyTerm expression <*> emptyTerm)

expressionCase :: Assignment Term
expressionCase = makeTerm <$> symbol ExpressionCase <*> (Statement.Pattern <$> children expressions <*> expressions)

expressionCaseClause :: Assignment Term
expressionCaseClause = symbol ExpressionCaseClause *> children (expressionCase <|> defaultExpressionCase)

expressionList :: Assignment Term
expressionList = symbol ExpressionList *> children expressions

expressionSwitchStatement :: Assignment Term
expressionSwitchStatement = makeTerm <$> symbol ExpressionSwitchStatement <*> children (Statement.Match <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ExpressionCaseClause)) <|> emptyTerm) <*> expressions)

fallThroughStatement :: Assignment Term
fallThroughStatement = makeTerm <$> symbol FallthroughStatement <*> (Statement.Pattern <$> (makeTerm <$> location <*> (Syntax.Identifier . name <$> source)) <*> emptyTerm)

functionDeclaration :: Assignment Term
functionDeclaration =  makeTerm <$> (symbol FunctionDeclaration <|> symbol FuncLiteral) <*> children (mkFunctionDeclaration <$> (term identifier <|> emptyTerm) <*> params <*> returnTypes <*> (term block <|> emptyTerm))
  where
    returnTypes =  pure <$> (term types <|> term identifier <|> term returnParameters)
               <|> pure []
    params = symbol ParameterList *> children (manyTerm expression)
    mkFunctionDeclaration name' params' types' block' = Declaration.Function types' name' params' block'
    returnParameters = makeTerm <$> symbol ParameterList <*> children (manyTerm expression)

importDeclaration :: Assignment Term
importDeclaration = makeTerm'' <$> symbol ImportDeclaration <*> children (manyTerm (importSpec <|> importSpecList))
  where
    -- `import . "lib/Math"`
    dotImport = inject <$> (flip Go.Syntax.Import <$> dot <*> importFromPath)
    -- `import _ "lib/Math"`
    sideEffectImport = inject <$> (flip Go.Syntax.SideEffectImport <$> underscore <*> importFromPath)
    -- `import m "lib/Math"`
    namedImport = inject <$> (flip Go.Syntax.QualifiedImport <$> packageIdentifier <*> importFromPath)
    -- `import "lib/Math"`
    plainImport = inject <$> (symbol InterpretedStringLiteral >>= \loc -> do
      from <- importPath <$> source
      let alias = makeTerm loc (Syntax.Identifier (defaultAlias from)) -- Go takes `import "lib/Math"` and uses `Math` as the qualified name (e.g. `Math.Sin()`)
      pure $! Go.Syntax.QualifiedImport from alias)
    interpretedStringLiteral' = symbol InterpretedStringLiteral *> children (  (inject . Literal.String <$> some escapeSequence)
                                                                           <|> (inject . Literal.TextElement <$> source))
    rawStringLiteral' = symbol RawStringLiteral *> (inject . Literal.TextElement <$> source)
    dot = makeTerm <$> symbol Dot <*> (Literal.TextElement <$> source)
    underscore = makeTerm <$> symbol BlankIdentifier <*> (Literal.TextElement <$> source)
    importSpec = makeTerm' <$> symbol ImportSpec <*> children (sideEffectImport <|> dotImport <|> namedImport <|> plainImport <|> interpretedStringLiteral' <|> rawStringLiteral')
    importSpecList = makeTerm <$> symbol ImportSpecList <*> children (manyTerm (importSpec <|> comment))
    importFromPath = symbol InterpretedStringLiteral *> (importPath <$> source)

indexExpression :: Assignment Term
indexExpression = makeTerm <$> symbol IndexExpression <*> children (Expression.Subscript <$> expression <*> manyTerm expression)

methodDeclaration :: Assignment Term
methodDeclaration = makeTerm <$> symbol MethodDeclaration <*> children (mkTypedMethodDeclaration <$> receiver <*> term fieldIdentifier <*> params <*> returnParameters <*> (term block <|> emptyTerm))
  where
    params = symbol ParameterList *> children (manyTerm expression)
    receiver = symbol ParameterList *> children expressions
    mkTypedMethodDeclaration receiver' name' parameters' type'' body' = Declaration.Method type'' receiver' name' parameters' body'
    returnParameters = (symbol ParameterList *> children (manyTerm expression))
                    <|> pure <$> expression
                    <|> pure []

methodSpec :: Assignment Term
methodSpec =  makeTerm <$> symbol MethodSpec <*> children (mkMethodSpec <$> expression <*> params <*> (expression <|> emptyTerm))
  where
    params = symbol ParameterList *> children (manyTerm expression)
    mkMethodSpec name' params optionalTypeLiteral = Declaration.MethodSignature [optionalTypeLiteral] name' params

methodSpecList :: Assignment Term
methodSpecList = symbol MethodSpecList *> children expressions

packageClause :: Assignment Term
packageClause = makeTerm <$> symbol PackageClause <*> children (Go.Syntax.Package <$> expression <*> pure [])

parameters :: Assignment Term
parameters = symbol ParameterList *> children expressions

parameterDeclaration :: Assignment Term
parameterDeclaration = makeTerm <$> symbol ParameterDeclaration <*> children (manyTerm expression)

parenthesizedExpression :: Assignment Term
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

selectorExpression :: Assignment Term
selectorExpression = makeTerm <$> symbol SelectorExpression <*> children (Expression.MemberAccess <$> expression <*> (identifier' <|> fieldIdentifier'))

sliceExpression :: Assignment Term
sliceExpression = makeTerm <$> symbol SliceExpression <*> children (Go.Syntax.Slice <$> expression <* token AnonLBracket <*> (emptyTerm <|> expression) <* token AnonColon <*> (expression <|> emptyTerm) <* optional (token AnonColon) <*> (expression <|> emptyTerm))

typeAssertion :: Assignment Term
typeAssertion = makeTerm <$> symbol TypeAssertionExpression <*> children (Go.Syntax.TypeAssertion <$> expression <*> expression)

typeCase :: Assignment Term
typeCase = symbol TypeCase *> children expressions

typeCaseClause :: Assignment Term
typeCaseClause = makeTerm <$> symbol TypeCaseClause <*> children (Statement.Pattern <$> expression <*> expressions)

typeConversion :: Assignment Term
typeConversion = makeTerm <$> symbol TypeConversionExpression <*> children (Go.Syntax.TypeConversion <$> expression <*> expression)

typeSwitchGuard :: Assignment Term
typeSwitchGuard = makeTerm <$> symbol Grammar.TypeSwitchGuard <*> children (Go.Syntax.TypeSwitchGuard <$> expressions)

typeSwitchStatement :: Assignment Term
typeSwitchStatement = makeTerm <$> symbol TypeSwitchStatement <*> children (Go.Syntax.TypeSwitch <$> typeSwitchSubject <*> expressions)
  where
    typeSwitchSubject = makeTerm <$> location <*> manyTermsTill expression (void (symbol TypeCaseClause)) <|> emptyTerm

unaryExpression :: Assignment Term
unaryExpression = makeTerm' <$> symbol UnaryExpression <*> (  notExpression
                                                          <|> unaryMinus
                                                          <|> unaryAmpersand
                                                          <|> unaryReceive
                                                          <|> unaryPointer
                                                          <|> unaryComplement
                                                          <|> unaryPlus )
  where
    notExpression   = inject <$> children (Expression.Not <$ symbol AnonBang <*> expression)
    unaryAmpersand  = inject <$> children (Literal.Reference <$ symbol AnonAmpersand <*> expression)
    unaryComplement = inject <$> children (Expression.Complement <$ symbol AnonCaret <*> expression)
    unaryMinus      = inject <$> children (Expression.Negate <$ symbol AnonMinus <*> expression)
    unaryPlus       =         children (symbol AnonPlus *> (Term.termOut <$> expression))
    unaryPointer    = inject <$> children (Literal.Pointer <$ symbol AnonStar <*> expression)
    unaryReceive    = inject <$> children (Go.Syntax.ReceiveOperator <$ symbol AnonLAngleMinus <*> expression)

varDeclaration :: Assignment Term
varDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

variadicArgument :: Assignment Term
variadicArgument = makeTerm <$> symbol VariadicArgument <*> children (Go.Syntax.Variadic [] <$> expression)

variadicParameterDeclaration :: Assignment Term
variadicParameterDeclaration =  makeTerm <$> symbol VariadicParameterDeclaration <*> children (flip Go.Syntax.Variadic <$> (expression <|> emptyTerm) <* token AnonDotDotDot <*> many expression)

varSpecification :: Assignment Term
varSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment [] <$> (annotatedLHS <|> identifiers) <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation <$> (makeTerm <$> location <*> manyTermsTill identifier (void (symbol TypeIdentifier))) <*> expression)


-- Statements

assignment' :: Assignment Term
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
    assign :: Term -> Term -> Sum Syntax Term
    assign l r = inject (Statement.Assignment [] l r)

    augmentedAssign :: (f :< Syntax) => (Term -> Term -> f Term) -> Term -> Term -> Sum Syntax Term
    augmentedAssign c l r = assign l (makeTerm1 (c l r))

    invert cons a b = Expression.Not (makeTerm1 (cons a b))

breakStatement :: Assignment Term
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (expression <|> emptyTerm))

communicationClause :: Assignment Term
communicationClause = makeTerm <$> symbol CommunicationClause <*> children (Statement.Pattern <$> (communicationCase <|> expression) <*> expressions)
  where
    communicationCase = symbol CommunicationCase *> children expression

continueStatement :: Assignment Term
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (expression <|> emptyTerm))

decStatement :: Assignment Term
decStatement = makeTerm <$> symbol DecStatement <*> children (Statement.PostDecrement <$> expression)

deferStatement :: Assignment Term
deferStatement = makeTerm <$> symbol DeferStatement <*> children (Go.Syntax.Defer <$> expression)

elseClause :: Assignment Term
elseClause = symbol ElseClause *> children expression

emptyStatement :: Assignment Term
emptyStatement = makeTerm <$> token EmptyStatement <*> (Statement.NoOp <$> emptyTerm)

forStatement :: Assignment Term
forStatement =  makeTerm' <$> symbol ForStatement <*> children (forClause <|> forSimpleClause <|> rangeClause)
  where
    forClause = inject <$> (symbol ForClause *> children (Statement.For <$> (expression <|> emptyTerm) <*> (expression <|> emptyTerm) <*> (expression <|> emptyTerm)) <*> expression)
    forSimpleClause = inject <$> (Statement.For <$> emptyTerm <*> (expression <|> emptyTerm) <*> emptyTerm <*> expression)
    rangeClause = inject <$> (symbol RangeClause *> children (Statement.ForEach <$> (expression <|> emptyTerm) <*> expression) <*> expression)

goStatement :: Assignment Term
goStatement = makeTerm <$> symbol GoStatement <*> children (Go.Syntax.Go <$> expression)

gotoStatement :: Assignment Term
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> expression)

ifStatement :: Assignment Term
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol Block))) <*> expression <*> (expression <|> emptyTerm))

ifInitializer :: Assignment Term
ifInitializer = symbol IfInitializer *> children expression

incStatement :: Assignment Term
incStatement = makeTerm <$> symbol IncStatement <*> children (Statement.PostIncrement <$> expression)

keyedElement :: Assignment Term
keyedElement = makeTerm <$> symbol KeyedElement <*> children (Literal.KeyValue <$> expression <*> expression)

labelName :: Assignment Term
labelName = makeTerm <$> symbol LabelName <*> (Syntax.Identifier . name <$> source)

labeledStatement :: Assignment Term
labeledStatement = makeTerm <$> (symbol LabeledStatement <|> symbol LabeledStatement') <*> children (Go.Syntax.Label <$> expression <*> (expression <|> emptyTerm))

returnStatement :: Assignment Term
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expression <|> emptyTerm))

receiveStatement :: Assignment Term
receiveStatement = makeTerm <$> symbol ReceiveStatement <*> children (Go.Syntax.Receive <$> (expression <|> emptyTerm) <*> expression)

shortVarDeclaration :: Assignment Term
shortVarDeclaration = makeTerm <$> symbol ShortVarDeclaration <*> children (Statement.Assignment [] <$> expression <*> expression)

selectStatement :: Assignment Term
selectStatement = makeTerm <$> symbol SelectStatement <*> children (Go.Syntax.Select <$> expressions)

sendStatement :: Assignment Term
sendStatement = makeTerm <$> symbol SendStatement <*> children (Go.Syntax.Send <$> expression <*> expression)


-- Helpers

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment Term
          -> Assignment Term
          -> [Assignment (Term -> Term -> Sum Syntax Term)]
          -> Assignment (Sum Syntax Term)
infixTerm = infixContext comment

-- | Match a series of terms or comments until a delimiter is matched
manyTermsTill :: Assignment Term
              -> Assignment b
              -> Assignment [Term]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment Term -> Assignment [Term]
manyTerm = many . term

-- | Match a term and contextualize any comments preceeding or proceeding the term.
term :: Assignment Term -> Assignment Term
term term' = contextualize comment term' <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
