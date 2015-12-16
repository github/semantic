module Haskell where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String

leaf :: String -> Parser String -> Parser (Term String Info)
leaf production parser = do
  from <- getPosition
  parsed <- parser
  to <- getPosition
  return $ Info (Range 0 0) (Set.singleton production) :< Leaf parsed

module' :: Parser (Term String Info)
module' = toTerm <$> string "module"
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

haskellParser :: Parser (Term String Info)
haskellParser = toTerm <$> many anyChar
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

runHaskellParser :: String -> IO (Term String Info)
runHaskellParser input = return . either errorToTerm id $ parse haskellParser "" input
  where errorToTerm _ = Info (Range 0 0) mempty :< Leaf "onoes"
