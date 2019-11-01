{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, TypeFamilies, TypeOperators #-}
module Language.Go.Assignment
( assignment
, Go.Syntax
, Grammar
, Go.Term(..)
) where

import Prologue

import           Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.ScopeGraph as ScopeGraph (AccessControl(..))
import           Data.Abstract.Name (name)
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
import           Language.Go.Syntax as Go.Syntax hiding (runeLiteral, labelName)
import           Language.Go.Term as Go
import           Language.Go.Type as Go.Type
import Data.ImportPath (importPath, defaultAlias)
import           TreeSitter.Go as Grammar

type Assignment = Assignment.Assignment [] Grammar

-- | Assignment from AST in Go's grammar onto a program in Go's syntax.
assignment :: Assignment (Term Loc)
assignment = handleError program <|> parseError

program :: Assignment (Term Loc)
program = makeTerm <$> symbol SourceFile <*> children (Statement.Statements <$> manyTerm expression)

expression :: Assignment (Term Loc)
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment (Term Loc)]
expressionChoices =
  [ argumentList
  , assignment'
  , boolean
  , binaryExpression
  , block
  , breakStatement
  , callExpression
  , communicationCase
  , compositeLiteral
  , continueStatement
  , varDeclaration
  , varSpecification
  , decStatement
  , defaultCase
  , defaultExpressionCase
  , deferStatement
  , element
  , emptyStatement
  , expressionCase
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
  , nil
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

types :: Assignment (Term Loc)
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
         , typeSwitchStatement
         ]

identifiers :: Assignment (Term Loc)
identifiers = makeTerm'' <$> location <*> manyTerm identifier

expressions :: Assignment (Term Loc)
expressions = makeTerm'' <$> location <*> manyTerm expression


-- Literals

comment :: Assignment (Term Loc)
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

compositeLiteral :: Assignment (Term Loc)
compositeLiteral = makeTerm <$> symbol CompositeLiteral <*> children (Go.Syntax.Composite <$> expression <*> expression)

element :: Assignment (Term Loc)
element = symbol Element *> children expression

fieldIdentifier :: Assignment (Term Loc)
fieldIdentifier = makeTerm <$> symbol FieldIdentifier <*> (Syntax.Identifier . name <$> source)

floatLiteral :: Assignment (Term Loc)
floatLiteral = makeTerm <$> symbol FloatLiteral <*> (Literal.Float <$> source)

identifier :: Assignment (Term Loc)
identifier =  makeTerm <$> (symbol Identifier <|> symbol Identifier' <|> symbol Identifier'') <*> (Syntax.Identifier . name <$> source)

imaginaryLiteral :: Assignment (Term Loc)
imaginaryLiteral = makeTerm <$> symbol ImaginaryLiteral <*> (Literal.Complex <$> source)

interpretedStringLiteral :: Assignment (Term Loc)
interpretedStringLiteral = makeTerm' <$> symbol InterpretedStringLiteral <*> children (  (inject . Literal.String <$> some escapeSequence)
                                                                                     <|> (inject . Literal.TextElement <$> source))

escapeSequence :: Assignment (Term Loc)
escapeSequence = makeTerm <$> symbol EscapeSequence <*> (Literal.EscapeSequence <$> source)

intLiteral :: Assignment (Term Loc)
intLiteral = makeTerm <$> symbol IntLiteral <*> (Literal.Integer <$> source)

literalValue :: Assignment (Term Loc)
literalValue = makeTerm <$> symbol LiteralValue <*> children (manyTerm expression)

packageIdentifier :: Assignment (Term Loc)
packageIdentifier = makeTerm <$> symbol PackageIdentifier <*> (Syntax.Identifier . name <$> source)

parenthesizedType :: Assignment (Term Loc)
parenthesizedType = makeTerm <$> symbol Grammar.ParenthesizedType <*> children (Type.Parenthesized <$> expression)

rawStringLiteral :: Assignment (Term Loc)
rawStringLiteral = makeTerm <$> symbol RawStringLiteral <*> (Literal.TextElement <$> source)

runeLiteral :: Assignment (Term Loc)
runeLiteral = makeTerm <$> symbol Grammar.RuneLiteral <*> (Go.Syntax.Rune <$> source)

typeIdentifier :: Assignment (Term Loc)
typeIdentifier = makeTerm <$> symbol TypeIdentifier <*> (Syntax.Identifier . name <$> source)

nil :: Assignment (Term Loc)
nil = makeTerm <$> symbol Nil <*> (Literal.Null <$ source)

boolean :: Assignment (Term Loc)
boolean =  makeTerm <$> token Grammar.True <*> pure Literal.true
       <|> makeTerm <$> token Grammar.False <*> pure Literal.false


-- Primitive Types

arrayType :: Assignment (Term Loc)
arrayType = makeTerm <$> symbol ArrayType <*> children (Type.Array . Just <$> expression <*> expression)

channelType :: Assignment (Term Loc)
channelType =  makeTerm' <$> symbol ChannelType <*> children (mkChannelType <$> optional (token AnonLAngleMinus) <* token AnonChan <*> optional (token AnonLAngleMinus) <*> expression)
  where
    mkChannelType :: Maybe a -> Maybe a -> b -> Sum Go.Syntax b
    mkChannelType receive send | Just _ <- receive = inject . Go.Type.ReceiveChannel
                               | Just _ <- send    = inject . Go.Type.SendChannel
                               | otherwise         = inject . Go.Type.BidirectionalChannel

fieldDeclaration :: Assignment (Term Loc)
fieldDeclaration =  mkFieldDeclarationWithTag <$> symbol FieldDeclaration <*> children ((,,) <$> (manyTermsTill expression (void (symbol TypeIdentifier)) <|> manyTerm expression) <*> optional expression <*> optional expression)
  where
    mkFieldDeclarationWithTag loc (fields, type', tag) | Just ty <- type', Just tag' <- tag = makeTerm loc (Go.Syntax.Field [ty, tag'] (makeTerm loc fields))
                                                       | Just ty <- type'                   = makeTerm loc (Go.Syntax.Field [ty] (makeTerm loc fields))
                                                       | Just tag' <- tag                   = makeTerm loc (Go.Syntax.Field [tag'] (makeTerm loc fields))
                                                       | otherwise                          = makeTerm loc (Go.Syntax.Field [] (makeTerm loc fields))

fieldDeclarationList :: Assignment (Term Loc)
fieldDeclarationList = symbol FieldDeclarationList *> children expressions

functionType :: Assignment (Term Loc)
functionType = makeTerm <$> symbol FunctionType <*> children (Type.Function <$> params <*> (expression <|> emptyTerm))
  where params = symbol ParameterList *> children (manyTerm expression)

implicitLengthArrayType :: Assignment (Term Loc)
implicitLengthArrayType = makeTerm <$> symbol ImplicitLengthArrayType <*> children (Type.Array Nothing <$> expression)

interfaceType :: Assignment (Term Loc)
interfaceType = makeTerm <$> symbol InterfaceType <*> children (Type.Interface <$> manyTerm expression)

mapType :: Assignment (Term Loc)
mapType = makeTerm <$> symbol MapType <*> children (Type.Map <$> expression <*> expression)

pointerType :: Assignment (Term Loc)
pointerType = makeTerm <$> symbol PointerType <*> children (Type.Pointer <$> expression)

qualifiedType :: Assignment (Term Loc)
qualifiedType = makeTerm <$> symbol QualifiedType <*> children (Expression.MemberAccess <$> expression <*> typeIdentifier)

sliceType :: Assignment (Term Loc)
sliceType = makeTerm <$> symbol SliceType <*> children (Type.Slice <$> expression)

structType :: Assignment (Term Loc)
structType = makeTerm <$> symbol StructType <*> children (Declaration.Constructor [] <$> emptyTerm <*> expressions)

typeAlias :: Assignment (Term Loc)
typeAlias = makeTerm <$> symbol TypeAlias <*> children (Declaration.TypeAlias [] <$> expression <*> expression)

typeDeclaration :: Assignment (Term Loc)
typeDeclaration = makeTerm <$> symbol TypeDeclaration <*> children (manyTerm ( (makeTerm <$> symbol TypeSpec <*> children (Declaration.Type <$> typeIdentifier <*> expression))
                                                                            <|> typeAlias ))



-- Expressions

argumentList :: Assignment (Term Loc)
argumentList = (symbol ArgumentList <|> symbol ArgumentList') *> children expressions

binaryExpression :: Assignment (Term Loc)
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

block :: Assignment (Term Loc)
block = symbol Block *> children expressions

defaultCase :: Assignment (Term Loc)
defaultCase = makeTerm <$> symbol DefaultCase <*> children (Go.Syntax.DefaultPattern <$> (expressions <|> emptyTerm))

defaultExpressionCase :: Assignment (Term Loc)
defaultExpressionCase = makeTerm <$> symbol DefaultCase <*> (Go.Syntax.DefaultPattern <$ rawSource <*> (expressions <|> emptyTerm))

callExpression :: Assignment (Term Loc)
callExpression = makeTerm <$> symbol CallExpression <*> children (Expression.Call [] <$> expression <*> manyTerm expression <*> emptyTerm)

expressionCase :: Assignment (Term Loc)
expressionCase = makeTerm <$> symbol ExpressionCase <*> (Statement.Pattern <$> children expressions <*> expressions)

expressionList :: Assignment (Term Loc)
expressionList = symbol ExpressionList *> children expressions

expressionSwitchStatement :: Assignment (Term Loc)
expressionSwitchStatement
  = makeTerm
  <$> symbol ExpressionSwitchStatement
  <*> children (Statement.Match <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol ExpressionCase)) <|> emptyTerm) <*> expressions)

fallThroughStatement :: Assignment (Term Loc)
fallThroughStatement = makeTerm <$> symbol FallthroughStatement <*> (Statement.Pattern <$> (makeTerm <$> location <*> (Syntax.Identifier . name <$> source)) <*> emptyTerm)

functionDeclaration :: Assignment (Term Loc)
functionDeclaration = makeTerm <$> (symbol FunctionDeclaration <|> symbol FuncLiteral) <*> children (mkFunctionDeclaration <$> (term identifier <|> emptyTerm) <*> params <*> returnTypes <*> (term block <|> emptyTerm))
  where
    returnTypes =  pure <$> (term types <|> term identifier <|> term returnParameters)
               <|> pure []
    params = symbol ParameterList *> children (manyTerm expression)
    mkFunctionDeclaration name' params' types' block' = Declaration.Function types' name' params' block'
    returnParameters = makeTerm <$> symbol ParameterList <*> children (manyTerm expression)

importDeclaration :: Assignment (Term Loc)
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

indexExpression :: Assignment (Term Loc)
indexExpression = makeTerm <$> symbol IndexExpression <*> children (Expression.Subscript <$> expression <*> manyTerm expression)

methodDeclaration :: Assignment (Term Loc)
methodDeclaration = makeTerm <$> symbol MethodDeclaration <*> children (mkTypedMethodDeclaration <$> receiver <*> pure publicAccessControl <*> term fieldIdentifier <*> params <*> returnParameters <*> (term block <|> emptyTerm))
  where
    params = symbol ParameterList *> children (manyTerm expression)
    receiver = symbol ParameterList *> children expressions
    mkTypedMethodDeclaration receiver' accessControl name' parameters' type'' body' = Declaration.Method type'' receiver' name' parameters' body' accessControl
    returnParameters = (symbol ParameterList *> children (manyTerm expression))
                    <|> pure <$> expression
                    <|> pure []

methodSpec :: Assignment (Term Loc)
methodSpec =  makeTerm <$> symbol MethodSpec <*> children (mkMethodSpec publicAccessControl <$> expression <*> params <*> (expression <|> emptyTerm))
  where
    params = symbol ParameterList *> children (manyTerm expression)
    mkMethodSpec accessControl name' params optionalTypeLiteral = Declaration.MethodSignature [optionalTypeLiteral] name' params accessControl

publicAccessControl :: ScopeGraph.AccessControl
publicAccessControl = ScopeGraph.Public

methodSpecList :: Assignment (Term Loc)
methodSpecList = symbol MethodSpecList *> children expressions

packageClause :: Assignment (Term Loc)
packageClause = makeTerm <$> symbol PackageClause <*> children (Go.Syntax.Package <$> expression <*> pure [])

parameters :: Assignment (Term Loc)
parameters = symbol ParameterList *> children expressions

parameterDeclaration :: Assignment (Term Loc)
parameterDeclaration = makeTerm <$> symbol ParameterDeclaration <*> children (manyTerm expression)

parenthesizedExpression :: Assignment (Term Loc)
parenthesizedExpression = symbol ParenthesizedExpression *> children expressions

selectorExpression :: Assignment (Term Loc)
selectorExpression = makeWithContext <$> symbol SelectorExpression <*> children ((,,) <$> expression <*> optional comment <*> fieldIdentifier)
  where makeWithContext loc (lhs, comment, rhs) = maybe (makeTerm loc (Expression.MemberAccess lhs rhs)) (\c -> makeTerm loc (Syntax.Context (c :| []) (makeTerm loc (Expression.MemberAccess lhs rhs)))) comment

sliceExpression :: Assignment (Term Loc)
sliceExpression = makeTerm <$> symbol SliceExpression <*> children (Go.Syntax.Slice <$> expression <* token AnonLBracket <*> (emptyTerm <|> expression) <* token AnonColon <*> (expression <|> emptyTerm) <* optional (token AnonColon) <*> (expression <|> emptyTerm))

typeAssertion :: Assignment (Term Loc)
typeAssertion = makeTerm <$> symbol TypeAssertionExpression <*> children (Go.Syntax.TypeAssertion <$> expression <*> expression)

typeCase :: Assignment (Term Loc)
typeCase = symbol TypeCase *> children expressions

typeConversion :: Assignment (Term Loc)
typeConversion = makeTerm <$> symbol TypeConversionExpression <*> children (Go.Syntax.TypeConversion <$> expression <*> expression)

typeSwitchStatement :: Assignment (Term Loc)
typeSwitchStatement = makeTerm <$> symbol TypeSwitchStatement <*> children (Go.Syntax.TypeSwitch <$> typeSwitchSubject <*> expressions)
  where
    typeSwitchSubject = makeTerm <$> location <*> manyTermsTill expression (void (symbol TypeCase)) <|> emptyTerm

unaryExpression :: Assignment (Term Loc)
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

varDeclaration :: Assignment (Term Loc)
varDeclaration = (symbol ConstDeclaration <|> symbol VarDeclaration) *> children expressions

variadicArgument :: Assignment (Term Loc)
variadicArgument = makeTerm <$> symbol VariadicArgument <*> children (Go.Syntax.Variadic [] <$> expressions)

variadicParameterDeclaration :: Assignment (Term Loc)
variadicParameterDeclaration =  makeTerm <$> symbol VariadicParameterDeclaration <*> children (flip Go.Syntax.Variadic <$> (expression <|> emptyTerm) <* token AnonDotDotDot <*> many expression)

varSpecification :: Assignment (Term Loc)
varSpecification = makeTerm <$> (symbol ConstSpec <|> symbol VarSpec) <*> children (Statement.Assignment [] <$> (annotatedLHS <|> identifiers) <*> expressions)
    where
      annotatedLHS = makeTerm <$> location <*> (Type.Annotation <$> (makeTerm <$> location <*> manyTermsTill identifier (void (symbol TypeIdentifier))) <*> expression)


-- Statements

assignment' :: Assignment (Term Loc)
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
    assign :: Term Loc -> Term Loc -> Sum Go.Syntax (Term Loc)
    assign l r = inject (Statement.Assignment [] l r)

    augmentedAssign :: (f :< Go.Syntax) => (Term Loc -> Term Loc -> f (Term Loc)) -> Term Loc -> Term Loc -> Sum Go.Syntax (Term Loc)
    augmentedAssign c l r = assign l (makeTerm1 (c l r))

    invert cons a b = Expression.Not (makeTerm1 (cons a b))

breakStatement :: Assignment (Term Loc)
breakStatement = makeTerm <$> symbol BreakStatement <*> children (Statement.Break <$> (expression <|> emptyTerm))

communicationCase :: Assignment (Term Loc)
communicationCase = makeTerm <$> symbol CommunicationCase <*> children (Statement.Pattern <$> expression <*> expressions)

continueStatement :: Assignment (Term Loc)
continueStatement = makeTerm <$> symbol ContinueStatement <*> children (Statement.Continue <$> (expression <|> emptyTerm))

decStatement :: Assignment (Term Loc)
decStatement = makeTerm <$> symbol DecStatement <*> children (Statement.PostDecrement <$> expression)

deferStatement :: Assignment (Term Loc)
deferStatement = makeTerm <$> symbol DeferStatement <*> children (Go.Syntax.Defer <$> expression)

emptyStatement :: Assignment (Term Loc)
emptyStatement = makeTerm <$> token EmptyStatement <*> (Statement.NoOp <$> emptyTerm)

forStatement :: Assignment (Term Loc)
forStatement =  makeTerm' <$> symbol ForStatement <*> children (forClause <|> forSimpleClause <|> rangeClause)
  where
    forClause = inject <$> (symbol ForClause *> children (Statement.For <$> (expression <|> emptyTerm) <*> (expression <|> emptyTerm) <*> (expression <|> emptyTerm)) <*> expression)
    forSimpleClause = inject <$> (Statement.For <$> emptyTerm <*> (expression <|> emptyTerm) <*> emptyTerm <*> expression)
    rangeClause = inject <$> (symbol RangeClause *> children (Statement.ForEach <$> (expression <|> emptyTerm) <*> expression) <*> expression)

goStatement :: Assignment (Term Loc)
goStatement = makeTerm <$> symbol GoStatement <*> children (Go.Syntax.Go <$> expression)

gotoStatement :: Assignment (Term Loc)
gotoStatement = makeTerm <$> symbol GotoStatement <*> children (Statement.Goto <$> expression)

ifStatement :: Assignment (Term Loc)
ifStatement = makeTerm <$> symbol IfStatement <*> children (Statement.If <$> (makeTerm <$> location <*> manyTermsTill expression (void (symbol Block))) <*> expression <*> (expression <|> emptyTerm))

incStatement :: Assignment (Term Loc)
incStatement = makeTerm <$> symbol IncStatement <*> children (Statement.PostIncrement <$> expression)

keyedElement :: Assignment (Term Loc)
keyedElement = makeTerm <$> symbol KeyedElement <*> children (Literal.KeyValue <$> expression <*> expression)

labelName :: Assignment (Term Loc)
labelName = makeTerm <$> symbol LabelName <*> (Syntax.Identifier . name <$> source)

labeledStatement :: Assignment (Term Loc)
labeledStatement = makeTerm <$> (symbol LabeledStatement <|> symbol LabeledStatement') <*> children (Go.Syntax.Label <$> expression <*> (expression <|> emptyTerm))

returnStatement :: Assignment (Term Loc)
returnStatement = makeTerm <$> symbol ReturnStatement <*> children (Statement.Return <$> (expression <|> emptyTerm))

receiveStatement :: Assignment (Term Loc)
receiveStatement = makeTerm <$> symbol ReceiveStatement <*> children (Go.Syntax.Receive <$> (expression <|> emptyTerm) <*> expression)

shortVarDeclaration :: Assignment (Term Loc)
shortVarDeclaration = makeTerm <$> symbol ShortVarDeclaration <*> children (Statement.Assignment [] <$> expression <*> expression)

selectStatement :: Assignment (Term Loc)
selectStatement = makeTerm <$> symbol SelectStatement <*> children (Go.Syntax.Select <$> expressions)

sendStatement :: Assignment (Term Loc)
sendStatement = makeTerm <$> symbol SendStatement <*> children (Go.Syntax.Send <$> expression <*> expression)


-- Helpers

-- | Match infix terms separated by any of a list of operators, assigning any comments following each operand.
infixTerm :: Assignment (Term Loc)
          -> Assignment (Term Loc)
          -> [Assignment (Term Loc -> Term Loc -> Sum Go.Syntax (Term Loc))]
          -> Assignment (Sum Go.Syntax (Term Loc))
infixTerm = infixContext comment

-- | Match a series of terms or comments until a delimiter is matched
manyTermsTill :: Assignment (Term Loc)
              -> Assignment b
              -> Assignment [Term Loc]
manyTermsTill step end = manyTill (step <|> comment) end

-- | Match a term optionally preceded by comment(s), or a sequence of comments if the term is not present.
manyTerm :: Assignment (Term Loc) -> Assignment [Term Loc]
manyTerm = many . term

-- | Match a term and contextualize any comments preceding or proceeding the term.
term :: Assignment (Term Loc) -> Assignment (Term Loc)
term term' = contextualize comment term' <|> makeTerm1 <$> (Syntax.Context <$> some1 comment <*> emptyTerm)
