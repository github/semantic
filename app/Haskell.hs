module Haskell where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified Data.Set as Set
import Text.Trifecta

leaf :: String -> Parser String -> Parser (Term String Info)
leaf production parser = do
  from <- position
  parsed <- parser
  to <- position
  return $ Info (Range 0 0) (Set.singleton production) :< Leaf parsed

module' :: Parser (Term String Info)
module' = toTerm <$> string "module"
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

haskellParser :: Parser (Term String Info)
haskellParser = toTerm <$> many anyChar
  where toTerm a = Info (Range 0 0) mempty :< Leaf a

runHaskellParser :: String -> IO (Term String Info)
runHaskellParser input = return $ case parseString haskellParser mempty input of
  Success a -> a
  Failure _ -> Info (Range 0 0) mempty :< Leaf "onoes"
