module Analysis.Syntax
( Syntax(..)
  -- * Pretty-printing
, Print(..)
) where

import Data.Text (Text)

class Syntax rep where
  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep


-- Pretty-printing

newtype Print = Print { print_ :: ShowS }

instance Semigroup Print where
  Print a <> Print b = Print (a . b)
