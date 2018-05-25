{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.Haskell.Assignment
( assignment
, Syntax
, Grammar
, Term
) where

import Assigning.Assignment hiding (Assignment, Error)
import Data.Record
import Data.Sum
import Data.Syntax (emptyTerm, handleError, parseError, makeTerm, makeTerm'', contextualize, postContextualize)
import Language.Haskell.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.Name as Name
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
  , Declaration.Function
  , Literal.Array
  , Literal.Character
  , Literal.Float
  , Literal.Integer
  , Literal.TextElement
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.FunctionConstructor
  , Syntax.Identifier
  , Syntax.ListConstructor
  , Syntax.Module
  , Syntax.TupleConstructor
  , Syntax.Type
  , Syntax.TypeSynonym
  , Syntax.UnitConstructor
  , Type.TypeParameters
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment' Term
type Assignment' a = HasCallStack => Assignment.Assignment [] Grammar a

assignment :: Assignment
assignment = handleError $ module' <|> parseError

module' :: Assignment
module' = makeTerm
       <$> symbol Module
       <*> children (Syntax.Module <$> (moduleIdentifier <|> emptyTerm) <*> pure [] <*> (where' <|> expressions <|> emptyTerm))


expressions :: Assignment
expressions = makeTerm'' <$> location <*> many expression

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices = [
                      character
                    , comment
                    , constructorIdentifier
                    , float
                    , functionConstructor
                    , functionDeclaration
                    , integer
                    , listConstructor
                    , listExpression
                    , listType
                    , moduleIdentifier
                    , string
                    , type'
                    , typeConstructorIdentifier
                    , typeSynonymDeclaration
                    , typeVariableIdentifier
                    , tuplingConstructor
                    , unitConstructor
                    , variableIdentifier
                    , where'
                    ]

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

variableIdentifier :: Assignment
variableIdentifier = makeTerm <$> symbol VariableIdentifier <*> (Syntax.Identifier . Name.name <$> source)

constructorIdentifier :: Assignment
constructorIdentifier = makeTerm <$> symbol ConstructorIdentifier <*> (Syntax.Identifier . Name.name <$> source)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.Identifier . Name.name <$> source)

typeConstructorIdentifier :: Assignment
typeConstructorIdentifier = makeTerm <$> symbol TypeConstructorIdentifier <*> (Syntax.Identifier . Name.name <$> source)

typeVariableIdentifier :: Assignment
typeVariableIdentifier = makeTerm <$> symbol TypeVariableIdentifier <*> (Syntax.Identifier . Name.name <$> source)

where' :: Assignment
where' = makeTerm <$> (symbol Where <|> symbol Where') <*> children (many expression)

functionBody :: Assignment
functionBody = makeTerm <$> symbol FunctionBody <*> children (many expression)

functionConstructor :: Assignment
functionConstructor = makeTerm <$> token FunctionConstructor  <*> (Syntax.FunctionConstructor <$> emptyTerm)

functionDeclaration :: Assignment
functionDeclaration = makeTerm
                   <$> symbol FunctionDeclaration
                   <*> children (Declaration.Function
                               <$> pure []
                               <*> variableIdentifier
                               <*> (manyTermsTill expression (symbol FunctionBody) <|> pure [])
                               <*> functionBody)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

listConstructor :: Assignment
listConstructor = makeTerm <$> token ListConstructor <*> (Syntax.ListConstructor <$> emptyTerm)

unitConstructor :: Assignment
unitConstructor = makeTerm <$> token UnitConstructor <*> (Syntax.UnitConstructor <$> emptyTerm)

listExpression :: Assignment
listExpression = makeTerm <$> symbol ListExpression <*> children (Literal.Array <$> many listElement)
  where listElement = symbol Expression *> children expression

listType :: Assignment
listType = makeTerm <$> symbol ListType <*> children (Literal.Array <$> many type')

tuplingConstructor :: Assignment
tuplingConstructor = makeTerm <$> symbol TuplingConstructor <*> children (Syntax.TupleConstructor <$> emptyTerm)

type' :: Assignment
type' = (makeTerm <$> symbol Type <*> children (Syntax.Type <$> typeConstructor <*> typeParameters))
     <|> (makeTerm <$> symbol TypePattern <*> children (Syntax.Type <$> typeConstructor <*> typeParameters))

typeParameters :: Assignment
typeParameters = makeTerm <$> location <*> (Type.TypeParameters <$> many expression)

float :: Assignment
float = makeTerm <$> symbol Float <*> (Literal.Float <$> source)

character :: Assignment
character = makeTerm <$> symbol Char <*> (Literal.Character <$> source)

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

typeConstructor :: Assignment
typeConstructor = typeConstructorIdentifier
               <|> functionConstructor
               <|> listConstructor
               <|> listType
               <|> tuplingConstructor
               <|> unitConstructor

typeSynonymDeclaration :: Assignment
typeSynonymDeclaration = makeTerm
                      <$> symbol TypeSynonymDeclaration
                      <*> children (Syntax.TypeSynonym <$> typeLeft <*> typeRight)
  where
    typeLeft = makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParametersLeft)
    typeParametersLeft = makeTerm <$> location <*> (Type.TypeParameters <$> manyTill expression (symbol TypeSynonymBody))
    typeRight = symbol TypeSynonymBody *> children type'

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step = manyTill (step <|> comment)
