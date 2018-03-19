module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  doctest (map ("-X" ++) extensions ++ "-isrc" : if null args then ["src"] else args)

extensions :: [String]
extensions =
  [ "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveTraversable"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "OverloadedStrings"
  , "RecordWildCards"
  , "StrictData"
  ]
