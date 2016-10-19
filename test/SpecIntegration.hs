{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prologue
import Prelude (String)
import Data.List
import Data.List.Utils
import qualified SemanticGitDiffSpec
import Test.Hspec
import System.Environment (withArgs)

main :: IO ()
main = do
  args <- getArgs
  let (language, rest) = parseCustomArgs args
  withArgs rest . hspec . parallel $ describe "DiffSummaries" (SemanticGitDiffSpec.spec language)
  where
    parseCustomArgs :: [String] -> (Maybe String, [String])
    parseCustomArgs args = case partitioned of
      (l:_, rest) -> (toLang l, rest)
      _ -> (Nothing, args)
      where
        partitioned = partition (startswith "--language") args
        toLang s = Just $ replace "--language=" "" s
