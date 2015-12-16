module Haskell where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import Text.Parsec
import Text.Parsec.String

data TokenInfo = TokenInfo { getRange :: Range, getString :: String }
data Token = Token TokenInfo

tokenize :: String -> [Token]
tokenize "" = []
tokenize string = Token . uncurry TokenInfo <$> rangesAndWordsFrom 0 string

module' :: Parser (Term String Info)
module' = toTerm <$> string "module"
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

haskellParser :: Parser (Term String Info)
haskellParser = toTerm <$> many anyChar
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

runHaskellParser :: String -> IO (Term String Info)
runHaskellParser input = return . either errorToTerm id $ parse haskellParser "" input
  where errorToTerm _ = Info (Range 0 0) mempty :< Leaf "onoes"
