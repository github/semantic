{-# LANGUAGE DataKinds, DeriveAnyClass, RankNTypes, TypeOperators #-}
module Language.Go.Syntax
( assignment
, Syntax
, Grammar
, Term
) where

import Algorithm
import Data.Align.Generic
import Data.Functor (void)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Record
import Data.Syntax (contextualize, emptyTerm, handleError, infixContext, makeTerm, makeTerm', makeTerm1, postContextualize)
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import Data.Union
import GHC.Generics
import GHC.Stack
import Language.Go.Grammar as Grammar
import qualified Term

type Syntax =
  '[ Declaration.Function
   , Declaration.Method
   , Syntax.Error
   , Syntax.Empty
   , Syntax.Program
   , []
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment [] Grammar Term

-- | Assignment from AST in Python's grammar onto a program in Python's syntax.
assignment :: Assignment
assignment = handleError $ makeTerm <$> symbol SourceFile <*> children (Syntax.Program <$> many expression)

expression :: Assignment
expression = emptyTerm
