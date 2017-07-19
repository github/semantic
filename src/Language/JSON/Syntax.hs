{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators #-}
module Language.JSON.Syntax
  ( assignment
  , Syntax
  , Grammar
  , Term)
  where

import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Literal as Literal
import Data.Syntax.Assignment hiding (Assignment, Error)
import qualified Data.Syntax.Assignment as Assignment
import Language.JSON.Grammar as Grammar
import qualified Term
import Data.Record
import Data.Union
import GHC.Stack
import Prologue hiding (Location)

type Syntax =
  '[ Literal.Hash
   , Syntax.Error
   ]

type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = HasCallStack => Assignment.Assignment (AST Grammar) Grammar Term


makeTerm :: (HasCallStack, f :< fs) => a -> f (Term.Term (Union fs) a) -> Term.Term (Union fs) a
makeTerm a f = cofree (a :< inj f)

parseError :: Assignment
parseError = makeTerm <$> symbol ParseError <*> (Syntax.Error [] <$ source)

assignment :: Assignment
assignment = object <|> array <|> parseError

object :: Assignment
object = makeTerm <$> symbol Object <*> children (Literal.Hash <$> many pairs)
  where pairs = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> expression <*> expression)

array :: Assignment
array = makeTerm <$> symbol Array <*> children (Literal.Array <$> many expression)



