module Language.JSON.Assignment
( assignment
, Syntax
, Grammar
, Term)
where

import Assigning.Assignment.Deterministic hiding (Assignment)
import Data.AST
import Data.Record
import Data.Sum
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
type Assignment = TermAssignment Syntax Grammar


assignment :: Assignment Term
assignment = value <|> parseError

value :: Assignment Term
value = branchNode Value (object <|> array <|> parseError)

jsonValue :: Assignment Term
jsonValue = object <|> array <|> number <|> string <|> boolean <|> none <|> parseError

object :: Assignment Term
object = toTerm (branchNode Object (Literal.Hash <$> many pairs))
  where pairs = toTerm (branchNode Pair (Literal.KeyValue <$> (number <|> string <|> parseError) <*> jsonValue)) <|> parseError

array :: Assignment Term
array = toTerm (branchNode Array (Literal.Array <$> many jsonValue))

number :: Assignment Term
number = toTerm (Literal.Float <$> leafNode Number)

string :: Assignment Term
string = toTerm (Literal.TextElement <$> leafNode String)

boolean :: Assignment Term
boolean =  toTerm
  (   leafNode Grammar.True  $> Literal.true
  <|> leafNode Grammar.False $> Literal.false)

none :: Assignment Term
none = toTerm (leafNode Null $> Literal.Null)
