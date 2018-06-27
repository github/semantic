{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Language.JSON.Assignment
( assignment
, Syntax
, Grammar
, Term)
where

import Assigning.Assignment hiding (Assignment, Error)
import qualified Assigning.Assignment as Assignment
import Data.Record
import Data.Sum
import Data.Syntax (makeTerm, parseError)
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Literal as Literal
import qualified Data.Term as Term
import Language.JSON.Grammar as Grammar

type Syntax =
  [ Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Float
  , Literal.KeyValue
  , Literal.Null
  , Literal.TextElement
  , Syntax.Error
  ]

type Term = Term.Term (Sum Syntax) (Record Location)
type Assignment = Assignment.Assignment [] Grammar Term


assignment :: Assignment
assignment = Syntax.handleError (value <|> parseError)

value :: Assignment
value = symbol Value *> children (object <|> array)

jsonValue :: Assignment
jsonValue = object <|> array <|> number <|> string <|> boolean <|> none

object :: Assignment
object = makeTerm <$> symbol Object <*> children (Literal.Hash <$> many pairs)
  where pairs = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> (number <|> string) <*> jsonValue)

array :: Assignment
array = makeTerm <$> symbol Array <*> children (Literal.Array <$> many jsonValue)

number :: Assignment
number = makeTerm <$> symbol Number <*> (Literal.Float <$> source)

string :: Assignment
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

boolean :: Assignment
boolean =  makeTerm <$> symbol Grammar.True  <*> (Literal.true <$ rawSource)
       <|> makeTerm <$> symbol Grammar.False <*> (Literal.false <$ rawSource)

none :: Assignment
none = makeTerm <$> symbol Null <*> (Literal.Null <$ rawSource)
