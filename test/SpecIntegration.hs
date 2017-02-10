{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prologue
import Prelude (String)
import Data.List
import Data.List.Utils
import qualified IntegrationFormatSpec
import Test.Hspec
import System.Environment (withArgs)

main :: IO ()
main = do
  args <- getArgs
  let (language, rest) = parseCustomArgs args
  withArgs rest . hspec . parallel $ describe "Integration Format Specs" (IntegrationFormatSpec.spec language)
  where
    parseCustomArgs :: [String] -> (Maybe String, [String])
    parseCustomArgs args = case partitioned of
      (l:_, rest) -> (toLang l, rest)
      _ -> (Nothing, args)
      where
        partitioned = partition (startswith "--language") args
        toLang s = Just $ replace "--language=" "" s
