{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Haskell.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error, count)
import Data.ByteString.Char8 (count)
import Data.Record
import Data.Sum
import Data.Syntax (emptyTerm, handleError, parseError, makeTerm, makeTerm1, makeTerm', makeTerm'', contextualize, postContextualize)
import Language.Haskell.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.Name as Name
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Type as Type
import qualified Data.Term as Term
import qualified Language.Haskell.Syntax as Syntax
import Prologue

type Syntax = '[
    Comment.Comment
  , Declaration.Constructor
  , Declaration.Datatype
  , Declaration.Function
  , Literal.Array
  , Literal.Character
  , Literal.Float
  , Literal.Integer
  , Literal.TextElement
  , Syntax.AllConstructors
  , Syntax.AnnotatedTypeVariable
  , Syntax.Class
  , Syntax.ConstructorOperator
  , Syntax.Context
  , Syntax.Context'
  , Syntax.Deriving
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Export
  , Syntax.Field
  , Syntax.FunctionConstructor
  , Syntax.FunctionType
  , Syntax.GADT
  , Syntax.GADTConstructor
  , Syntax.Identifier
  , Syntax.InfixOperatorPattern
  , Syntax.Kind
  , Syntax.KindFunctionType
  , Syntax.KindListType
  , Syntax.KindSignature
  , Syntax.ListConstructor
  , Syntax.Module
  , Syntax.ModuleExport
  , Syntax.Pragma
  , Syntax.QualifiedModuleIdentifier
  , Syntax.QualifiedTypeConstructorIdentifier
  , Syntax.QuotedName
  , Syntax.RecordDataConstructor
  , Syntax.ScopedTypeVariables
  , Syntax.Star
  , Syntax.StrictType
  , Syntax.StrictTypeVariable
  , Syntax.TupleConstructor
  , Syntax.Type
  , Syntax.TypeConstructorExport
  , Syntax.TypePattern
  , Syntax.TypeSignature
  , Syntax.TypeSynonym
  , Syntax.UnitConstructor
  , Syntax.VariableOperator
  , Type.TypeParameters
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment' Term
type Assignment' a = HasCallStack => Assignment.Assignment [] Grammar a

assignment :: Assignment
assignment = handleError $ module' <|> parseError

algebraicDatatypeDeclaration :: Assignment
algebraicDatatypeDeclaration = makeTerm
                            <$> symbol AlgebraicDatatypeDeclaration
                            <*> children (Declaration.Datatype
                                        <$> (context' <|> emptyTerm)
                                        <*> (makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParameters <*> (kindSignature <|> emptyTerm)))
                                        <*> ((symbol Constructors *> children (manyTerm constructor))
                                            <|> pure [])
                                        <*> (derivingClause <|> emptyTerm))

allConstructors :: Assignment
allConstructors = makeTerm <$> token AllConstructors <*> pure Syntax.AllConstructors

annotatedTypeVariable :: Assignment
annotatedTypeVariable = makeTerm <$> symbol AnnotatedTypeVariable <*> children (Syntax.AnnotatedTypeVariable <$> typeVariableIdentifier <* token Annotation <*> (kind <|> type'))

character :: Assignment
character = makeTerm <$> symbol Char <*> (Literal.Character <$> source)

class' :: Assignment
class' = makeTerm <$> symbol Class <*> children (Syntax.Class <$> typeConstructor <*> typeParameters)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

constructor :: Assignment
constructor =  (makeTerm <$> symbol DataConstructor <*> children (Declaration.Constructor <$> (context' <|> emptyTerm) <*> typeConstructor <*> typeParameters))
           <|> (makeTerm <$> symbol RecordDataConstructor <*> children (Syntax.RecordDataConstructor <$> constructorIdentifier <*> fields))

constructorIdentifier :: Assignment
constructorIdentifier = makeTerm <$> symbol ConstructorIdentifier <*> (Syntax.Identifier . Name.name <$> source)

constructorOperator :: Assignment
constructorOperator = makeTerm <$> symbol ConstructorOperator <*> children (Syntax.ConstructorOperator <$> expression)

constructorSymbol :: Assignment
constructorSymbol = makeTerm <$> symbol ConstructorSymbol <*> (Syntax.Identifier . Name.name <$> source)

context' :: Assignment
context' = makeTerm <$> symbol Context <*> children (Syntax.Context' <$> manyTerm (type' <|> contextPattern))

contextPattern :: Assignment
contextPattern = symbol ContextPattern *> children type'

derivingClause :: Assignment
derivingClause = makeTerm <$> symbol Deriving <*> children (Syntax.Deriving <$> manyTerm typeConstructor)

export :: Assignment
export = makeTerm <$> symbol Export <*> children (Syntax.Export <$> expressions)

expressions :: Assignment
expressions = makeTerm'' <$> location <*> manyTerm expression

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices = [
                      algebraicDatatypeDeclaration
                    , allConstructors
                    , annotatedTypeVariable
                    , character
                    , comment
                    , context'
                    , constructorIdentifier
                    , constructorOperator
                    , constructorSymbol
                    , derivingClause
                    , float
                    , functionConstructor
                    , functionDeclaration
                    , functionType
                    , gadtConstructor
                    , gadtDeclaration
                    , infixOperatorPattern
                    , integer
                    , kind
                    , kindSignature
                    , listConstructor
                    , listExpression
                    , listType
                    , moduleExport
                    , moduleIdentifier
                    , operator
                    , parenthesizedTypePattern
                    , pragma
                    , qualifiedModuleIdentifier
                    , qualifiedTypeConstructorIdentifier
                    , quotedName
                    , scopedTypeVariables
                    , star
                    , strictType
                    , string
                    , type'
                    , type''
                    , typePattern
                    , typeConstructorExport
                    , typeConstructorIdentifier
                    , typeOperator
                    , typeSignature
                    , typeSynonymDeclaration
                    , typeVariableIdentifier
                    , tuplingConstructor
                    , unitConstructor
                    , variableIdentifier
                    , variableOperator
                    , variableSymbol
                    , where'
                    ]

fields :: Assignment
fields = makeTerm <$> symbol Fields <*> children (manyTerm field)

field :: Assignment
field = makeTerm
     <$> symbol Field
     <*> children (Syntax.Field
                 <$> variableIdentifiers
                 <* token Annotation
                 <*> fieldType)
  where
    fieldType = makeTerm <$> location <*> (Syntax.Type <$> term (type' <|> typeVariableIdentifier) <*> typeParameters <*> (kindSignature <|> emptyTerm))

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

functionBody :: Assignment
functionBody = makeTerm <$> symbol FunctionBody <*> children (manyTerm expression)

functionConstructor :: Assignment
functionConstructor = makeTerm <$> token FunctionConstructor  <*> pure Syntax.FunctionConstructor

functionDeclaration :: Assignment
functionDeclaration = makeTerm
                   <$> symbol FunctionDeclaration
                   <*> children (Declaration.Function
                               <$> pure []
                               <*> variableIdentifier
                               <*> (manyTermsTill expression (symbol FunctionBody) <|> pure [])
                               <*> functionBody)

functionType :: Assignment
functionType = makeTerm <$> symbol FunctionType <*> children (Syntax.FunctionType <$> type' <*> type')

gadtConstructor :: Assignment
gadtConstructor = makeTerm
               <$> symbol GadtConstructor
               <*> children (Syntax.GADTConstructor
                           <$> (context' <|> emptyTerm)
                           <*> typeConstructor
                           <* token Annotation
                           <*> term type')

gadtDeclaration :: Assignment
gadtDeclaration = makeTerm
               <$> symbol GadtDeclaration
               <*> children (Syntax.GADT
                           <$> (context' <|> emptyTerm)
                           <*> (makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParameters' <*> (kindSignature <|> emptyTerm)))
                           <*> where')
  where
    typeParameters' = makeTerm <$> location <*> manyTermsTill expression (symbol KindSignature <|> symbol Where')

infixOperatorPattern :: Assignment
infixOperatorPattern = makeTerm <$> symbol InfixOperatorPattern <*> children (Syntax.InfixOperatorPattern <$> expression <*> operator <*> expression)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

kind :: Assignment
kind = kind'
    <|> kindFunctionType
    <|> kindListType
    <|> star

kind' :: Assignment
kind' = makeTerm <$> symbol Kind <*> children (Syntax.Kind <$> kind)

kindFunctionType :: Assignment
kindFunctionType = makeTerm <$> symbol KindFunctionType <*> children (Syntax.KindFunctionType <$> kind <*> kind)

kindListType :: Assignment
kindListType = makeTerm <$> symbol KindListType <*> children (Syntax.KindListType <$> kind)

kindSignature :: Assignment
kindSignature = makeTerm <$> symbol KindSignature <*> children (Syntax.KindSignature <$ token Annotation <*> kind)

listConstructor :: Assignment
listConstructor = makeTerm <$> token ListConstructor <*> pure Syntax.ListConstructor

listExpression :: Assignment
listExpression = makeTerm <$> symbol ListExpression <*> children (Literal.Array <$> manyTerm listElement)
  where listElement = symbol Expression *> children expression

listType :: Assignment
listType = makeTerm <$> symbol ListType <*> children (Literal.Array <$> manyTerm type')

module' :: Assignment
module' = makeTerm
       <$> symbol Module
       <*> children (Syntax.Module <$> (term moduleIdentifier <|> emptyTerm) <*> moduleExports <*> (where' <|> expressions <|> emptyTerm))
  where
    moduleExports = symbol ModuleExports *> children (manyTerm export)
                 <|> pure []

moduleExport :: Assignment
moduleExport = makeTerm <$> symbol ModuleExport <*> children (Syntax.ModuleExport <$> expressions)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.Identifier . Name.name <$> source)

operator :: Assignment
operator = typeOperator <|> constructorOperator <|> variableOperator

parenthesizedTypePattern :: Assignment
parenthesizedTypePattern = symbol ParenthesizedTypePattern *> children expressions

pragma :: Assignment
pragma = makeTerm <$> symbol Pragma <*> (Syntax.Pragma <$> source)

qualifiedModuleIdentifier :: Assignment
qualifiedModuleIdentifier = makeTerm <$> symbol QualifiedModuleIdentifier <*> children (Syntax.QualifiedModuleIdentifier <$> someTerm' expression)

qualifiedTypeConstructorIdentifier :: Assignment
qualifiedTypeConstructorIdentifier = makeTerm <$> symbol QualifiedTypeConstructorIdentifier <*> children (Syntax.QualifiedTypeConstructorIdentifier <$> someTerm' expression)

quotedName :: Assignment
quotedName = makeTerm <$> symbol QuotedName <*> children (Syntax.QuotedName <$> expression)

scopedTypeVariables :: Assignment
scopedTypeVariables = makeTerm <$> symbol ScopedTypeVariables <*> children (Syntax.ScopedTypeVariables <$> expressions <* token Dot)

star :: Assignment
star = makeTerm <$> token Star <*> pure Syntax.Star

strictType :: Assignment
strictType = makeTerm'
          <$> symbol StrictType
          <*> children (  (inject <$> (Syntax.StrictType <$> typeConstructor <*> typeParameters))
                      <|> (inject <$> (Syntax.StrictTypeVariable <$> typeVariableIdentifier)))

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

typeClassIdentifier :: Assignment
typeClassIdentifier = makeTerm <$> symbol TypeClassIdentifier <*> (Syntax.Identifier . Name.name <$> source)

typeConstructorExport :: Assignment
typeConstructorExport = makeTerm <$> symbol TypeConstructorExport <*> children (Syntax.TypeConstructorExport <$> expression)

typeConstructorIdentifier :: Assignment
typeConstructorIdentifier = makeTerm <$> symbol TypeConstructorIdentifier <*> (Syntax.Identifier . Name.name <$> source)

typeOperator :: Assignment
typeOperator = makeTerm <$> symbol TypeOperator <*> (Syntax.Identifier . Name.name <$> source)

typeSignature :: Assignment
typeSignature = makeTerm <$> symbol TypeSignature <*> children (Syntax.TypeSignature <$> variableIdentifier <* token Annotation <*> (manyTerm context' <|> pure []) <*> type')

typeVariableIdentifier :: Assignment
typeVariableIdentifier = makeTerm <$> symbol TypeVariableIdentifier <*> (Syntax.Identifier . Name.name <$> source)

tuplingConstructor :: Assignment
tuplingConstructor = makeTerm <$> symbol TuplingConstructor <*> (tupleWithArity <$> rawSource)
        -- a tuple (,) has arity two, but only one comma, so apply the successor to the count of commas for the correct arity.
  where tupleWithArity = Syntax.TupleConstructor . succ . count ','

type' :: Assignment
type' =  class'
     <|> fields
     <|> functionType
     <|> parenthesizedTypePattern
     <|> strictType
     <|> type''
     <|> typeConstructor
     <|> typePattern

type'' :: Assignment
type'' = makeTerm
      <$> symbol Type
      <*> children (Syntax.Type <$> expression <*> typeParameters <*> (kindSignature <|> emptyTerm))

typeParameters :: Assignment
typeParameters = makeTerm <$> location <*> (Type.TypeParameters <$> (manyTermsTill expression (symbol Annotation) <|> many expression))

typePattern :: Assignment
typePattern = makeTerm <$> symbol TypePattern <*> children (Syntax.TypePattern <$> expressions)

typeConstructor :: Assignment
typeConstructor =  constructorIdentifier
               <|> functionConstructor
               <|> listConstructor
               <|> listType
               <|> qualifiedModuleIdentifier
               <|> qualifiedTypeConstructorIdentifier
               <|> quotedName
               <|> typeClassIdentifier
               <|> typeConstructorIdentifier
               <|> tuplingConstructor
               <|> unitConstructor

typeSynonymDeclaration :: Assignment
typeSynonymDeclaration = makeTerm
                      <$> symbol TypeSynonymDeclaration
                      <*> children (typeSynonym <$> typeLeft <*> typeRight)
  where
    typeLeft = makeTerm <$> location <*> manyTill expression (symbol TypeSynonymBody)
    typeRight = symbol TypeSynonymBody *> children ((,) <$> manyTerm (context' <|> scopedTypeVariables) <*> expression)
    typeSynonym typeLeft (contexts, typeRight) = Syntax.TypeSynonym typeLeft contexts typeRight

unitConstructor :: Assignment
unitConstructor = makeTerm <$> token UnitConstructor <*> pure Syntax.UnitConstructor

variableIdentifier :: Assignment
variableIdentifier = makeTerm <$> symbol VariableIdentifier <*> (Syntax.Identifier . Name.name <$> source)

variableOperator :: Assignment
variableOperator = makeTerm <$> symbol VariableOperator <*> children (Syntax.VariableOperator <$> expression)

variableSymbol :: Assignment
variableSymbol = makeTerm <$> (symbol VariableSymbol <|> symbol VariableSymbol') <*> (Syntax.Identifier . Name.name <$> source)

variableIdentifiers :: Assignment
variableIdentifiers = makeTerm <$> location <*> many variableIdentifier

where' :: Assignment
where' = makeTerm <$> (symbol Where <|> symbol Where') <*> children (manyTerm expression)

-- | Helpers

commentedTerm :: Assignment -> Assignment
commentedTerm term = contextualize (comment <|> pragma) term <|> makeTerm1 <$> (Syntax.Context <$> some1 (comment <|> pragma) <*> emptyTerm)

manyTerm :: Assignment -> Assignment.Assignment [] Grammar [Term]
manyTerm = many . commentedTerm

manyTermsTill :: Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step = manyTill (step <|> comment)

someTerm' :: Assignment -> Assignment.Assignment [] Grammar (NonEmpty Term)
someTerm' = NonEmpty.some1 . commentedTerm

term :: Assignment -> Assignment
term term = contextualize (comment <|> pragma) (postContextualize (comment <|> pragma) term)
