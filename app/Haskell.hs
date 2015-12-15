module Haskell where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import Text.Parsec
import Text.Parsec.String

haskellParser :: Parser (Term String Info)
haskellParser = toTerm <$> many anyChar
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

runHaskellParser :: String -> IO (Term String Info)
runHaskellParser input = return . either errorToTerm id $ parse haskellParser "" input
  where errorToTerm _ = Info (Range 0 0) mempty :< Leaf "_"
