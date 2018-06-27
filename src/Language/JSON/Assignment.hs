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
import Prologue

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
type Assignment = Assignment.Assignment [] Grammar


assignment :: Assignment Term
assignment = Syntax.handleError (value <|> parseError)

value :: Assignment Term
value = symbol Value *> children (object <|> array)

jsonValue :: Assignment Term
jsonValue = object <|> array <|> number <|> string <|> boolean <|> none

object :: Assignment Term
object = makeTerm <$> symbol Object <*> children (Literal.Hash <$> many pairs)
  where pairs = makeTerm <$> symbol Pair <*> children (Literal.KeyValue <$> (number <|> string) <*> jsonValue)

array :: Assignment Term
array = makeTerm <$> symbol Array <*> children (Literal.Array <$> many jsonValue)

number :: Assignment Term
number = makeTerm <$> symbol Number <*> (Literal.Float <$> source)

string :: Assignment Term
string = makeTerm <$> symbol String <*> (Literal.TextElement <$> source)

boolean :: Assignment Term
boolean =  toTerm
  (   leafNode Grammar.True  $> Literal.true
  <|> leafNode Grammar.False $> Literal.false)

none :: Assignment Term
none = toTerm (leafNode Null $> Literal.Null)
