module Analysis.Syntax
( Syntax(..)
) where

import Data.Text (Text)

class Syntax rep where
  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep
