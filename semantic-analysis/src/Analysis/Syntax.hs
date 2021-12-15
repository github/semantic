module Analysis.Syntax
( Syntax(..)
  -- * Pretty-printing
, Print(..)
  -- * Parsing
, Tree(..)
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Text (Text, pack, unpack)

class Syntax rep where
  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep


-- Pretty-printing

newtype Print = Print { print_ :: ShowS }

instance Show Print where
  showsPrec _ = print_

instance Semigroup Print where
  Print a <> Print b = Print (a . b)

instance Monoid Print where
  mempty = Print id

instance Syntax Print where
  iff c t e = parens (str "iff" <+> c <+> str "then" <+> t <+> str "else" <+> e)
  noop = parens (str "noop")

  bool b = parens (str (if b then "true" else "false"))
  string = parens . text

  throw e = parens (str "throw" <+> e)

str :: String -> Print
str = Print . showString

text :: Text -> Print
text = str . unpack

char :: Char -> Print
char = Print . showChar

parens :: Print -> Print
parens p = char '(' <> p <> char ')'

(<+>) :: Print -> Print -> Print
l <+> r = l <> char ' ' <> r

infixr 6 <+>


-- Parsing

-- Temporary until I can figure out how to have aeson construct a rep directly.

data Tree
  = NoOp
  | String Text
  | Bool Bool
  | If Tree Tree Tree
  | Throw Tree

instance A.FromJSON Tree where
  parseJSON = A.withObject "Tree" $ \ o -> do
    attrs <- o A..: pack "attrs"
    t <- attrs A..: pack "type"
    case t of
      "string" -> String <$> attrs A..: pack "text"
      "true"   -> pure (Bool True)
      "false"  -> pure (Bool False)
      "throw"  -> Throw . (!! 0) <$> o A..: pack "edges"
      _        -> A.parseFail ("unrecognized type: " <> t)
