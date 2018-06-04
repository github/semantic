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
import Data.Syntax (emptyTerm, handleError, parseError, makeTerm, makeTerm', makeTerm'', contextualize, postContextualize)
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
  , Declaration.Constructor
  , Declaration.Datatype
  , Declaration.Function
  , Literal.Array
  , Literal.Character
  , Literal.Float
  , Literal.Integer
  , Literal.TextElement
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Field
  , Syntax.FunctionConstructor
  , Syntax.Identifier
  , Syntax.ListConstructor
  , Syntax.Module
  , Syntax.Pragma
  , Syntax.RecordDataConstructor
  , Syntax.StrictType
  , Syntax.StrictTypeVariable
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
                      algebraicDatatypeDeclaration
                    , character
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
                    , strictType
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
term term = contextualize (comment <|> pragma) (postContextualize (comment <|> pragma) term)

algebraicDatatypeDeclaration :: Assignment
algebraicDatatypeDeclaration = makeTerm
                            <$> symbol AlgebraicDatatypeDeclaration
                            <*> children (Declaration.Datatype
                                        <$> (makeTerm <$> location <*> (Syntax.Type <$> typeConstructor <*> typeParameters))
                                        <*> ((symbol Constructors *> children (many constructor))
                                            <|> pure []))

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

constructor :: Assignment
constructor =  (makeTerm <$> symbol DataConstructor <*> children (Declaration.Constructor <$> typeConstructor <*> typeParameters))
           <|> (makeTerm <$> symbol RecordDataConstructor <*> children (Syntax.RecordDataConstructor <$> constructorIdentifier <*> fields))

fields :: Assignment
fields = makeTerm <$> symbol Fields <*> children (many field)

field :: Assignment
field = makeTerm <$> symbol Field <*> children (Syntax.Field <$> variableIdentifiers <* token Annotation <*> term type')

variableIdentifier :: Assignment
variableIdentifier = makeTerm <$> symbol VariableIdentifier <*> (Syntax.Identifier . Name.name <$> source)

variableIdentifiers :: Assignment
variableIdentifiers = makeTerm <$> location <*> many variableIdentifier

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
functionConstructor = makeTerm <$> token FunctionConstructor  <*> pure Syntax.FunctionConstructor

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
listConstructor = makeTerm <$> token ListConstructor <*> pure Syntax.ListConstructor

pragma :: Assignment
pragma = makeTerm <$> symbol Pragma <*> (Syntax.Pragma <$> source)

unitConstructor :: Assignment
unitConstructor = makeTerm <$> token UnitConstructor <*> pure Syntax.UnitConstructor

listExpression :: Assignment
listExpression = makeTerm <$> symbol ListExpression <*> children (Literal.Array <$> many listElement)
  where listElement = symbol Expression *> children expression

listType :: Assignment
listType = makeTerm <$> symbol ListType <*> children (Literal.Array <$> many type')

strictType :: Assignment
strictType = makeTerm' <$> symbol StrictType <*> children ((inject <$> (Syntax.StrictType <$> typeConstructor <*> typeParameters))
                                                        <|> (inject <$> (Syntax.StrictTypeVariable <$> typeVariableIdentifier)))

tuplingConstructor :: Assignment
tuplingConstructor = makeTerm <$> symbol TuplingConstructor <*> (tupleWithArity <$> rawSource)
        -- a tuple (,) has arity two, but only one comma, so apply the successor to the count of commas for the correct arity.
  where tupleWithArity = Syntax.TupleConstructor . succ . count ','

type' :: Assignment
type' = (makeTerm <$> symbol Type <*> children (Syntax.Type <$> typeConstructor <*> typeParameters))
     <|> (makeTerm <$> symbol TypePattern <*> children (Syntax.Type <$> typeConstructor <*> typeParameters))
     <|> strictType
     <|> typeConstructor

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
               <|> constructorIdentifier

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
