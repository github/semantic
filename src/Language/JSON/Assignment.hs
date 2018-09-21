{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.JSON.Assignment
( assignment
, Syntax
, Grammar
, Term)
where

import Assigning.Assignment.Deterministic hiding (Assignment)
import qualified Assigning.Assignment.Deterministic as Deterministic
import Data.Sum
import Data.Location
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Literal as Literal
import qualified Data.Term as Term
import qualified Data.Diff as Diff
import Language.JSON.Grammar as Grammar
import Proto3.Suite (Named1(..), Named(..))
import Prologue
import Text.Parser.Combinators

type Syntax =
  [ Literal.Null
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Float
  , Literal.KeyValue
  , Literal.TextElement
  , Syntax.Error
  ]

type Term = Term.Term (Sum Syntax) Location
type Assignment = Deterministic.Assignment Grammar

instance Named1 (Sum Syntax) where
  nameOf1 _ = "JSONSyntax"

instance Named (Term.Term (Sum Syntax) ()) where
  nameOf _ = "JSONTerm"

instance Named (Diff.Diff (Sum Syntax) () ()) where
  nameOf _ = "JSONDiff"


assignment :: Assignment Term
assignment = value <|> parseError

value :: Assignment Term
value = branchNode Value (object <|> array <|> parseError)

jsonValue :: Assignment Term
jsonValue = object <|> array <|> number <|> string <|> boolean <|> none <|> parseError

object :: Assignment Term
object = toTerm (branchNode Object (Literal.Hash <$ leafNode AnonLBrace <*> sepBy pairs (leafNode AnonComma) <* leafNode AnonRBrace))
  where pairs = toTerm (branchNode Pair (Literal.KeyValue <$> (number <|> string <|> parseError) <* leafNode AnonColon <*> jsonValue)) <|> parseError

array :: Assignment Term
array = toTerm (branchNode Array (Literal.Array <$ leafNode AnonLBracket <*> sepBy jsonValue (leafNode AnonComma) <* leafNode AnonRBracket))

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
