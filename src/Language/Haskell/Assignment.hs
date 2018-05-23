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
import qualified Data.Abstract.FreeVariables as FV
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Term as Term
import qualified Language.Haskell.Syntax as Syntax
import Prologue

type Syntax = '[
    Comment.Comment
  , Declaration.Function
  , Literal.Integer
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Module
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
                      comment
                    , constructorIdentifier
                    , functionDeclaration
                    , integer
                    , moduleIdentifier
                    , variableIdentifier
                    , where'
                    ]

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

variableIdentifier :: Assignment
variableIdentifier = makeTerm <$> symbol VariableIdentifier <*> (Syntax.Identifier . FV.name <$> source)

constructorIdentifier :: Assignment
constructorIdentifier = makeTerm <$> symbol ConstructorIdentifier <*> (Syntax.Identifier . FV.name <$> source)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.Identifier . FV.name <$> source)

where' :: Assignment
where' = makeTerm <$> (symbol Where <|> symbol Where') <*> children (many expression)

functionBody :: Assignment
functionBody = makeTerm <$> symbol FunctionBody <*> children (many expression)

functionDeclaration :: Assignment
functionDeclaration = makeTerm
                   <$> symbol FunctionDeclaration
                   <*> children (Declaration.Function
                               <$> pure []
                               <*> variableIdentifier
                               <*> ((manyTermsTill expression (symbol FunctionBody)) <|> pure [])
                               <*> functionBody)

integer :: Assignment
integer = makeTerm <$> symbol Integer <*> (Literal.Integer <$> source)

-- | Match a series of terms or comments until a delimiter is matched.
manyTermsTill :: Assignment.Assignment [] Grammar Term -> Assignment.Assignment [] Grammar b -> Assignment.Assignment [] Grammar [Term]
manyTermsTill step end = manyTill (step <|> comment) end
