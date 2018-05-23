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
import Data.Syntax (handleError, parseError, makeTerm, contextualize, postContextualize)
import Language.Haskell.Grammar as Grammar
import qualified Assigning.Assignment as Assignment
import qualified Data.Abstract.FreeVariables as FV
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Term as Term
import qualified Language.Haskell.Syntax as Syntax
import Prologue

type Syntax = '[
    Comment.Comment
  , Syntax.Context
  , Syntax.Empty
  , Syntax.Error
  , Syntax.Identifier
  , Syntax.Module
  , Syntax.Program
  , []
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment' a = HasCallStack => Assignment.Assignment [] Grammar a
type Assignment = Assignment' Term

assignment :: Assignment
assignment = handleError $ module' <|> parseError

module' :: Assignment
module' = makeTerm <$> symbol Module <*> children (Syntax.Module <$> moduleIdentifier <*> pure [] <*> (where' <|> pure []))

expression :: Assignment
expression = term (handleError (choice expressionChoices))

expressionChoices :: [Assignment.Assignment [] Grammar Term]
expressionChoices = [
                      constructorIdentifier
                    , moduleIdentifier
                    , comment
                    ]

term :: Assignment -> Assignment
term term = contextualize comment (postContextualize comment term)

comment :: Assignment
comment = makeTerm <$> symbol Comment <*> (Comment.Comment <$> source)

constructorIdentifier :: Assignment
constructorIdentifier = makeTerm <$> symbol ConstructorIdentifier <*> (Syntax.Identifier . FV.name <$> source)

moduleIdentifier :: Assignment
moduleIdentifier = makeTerm <$> symbol ModuleIdentifier <*> (Syntax.Identifier . FV.name <$> source)

where' :: Assignment' [Term]
where' = (symbol Where <|> symbol Where') *> children (many expression)
