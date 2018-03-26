module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  doctest (map ("-X" ++) extensions ++ "-isrc" : "--fast" : if null args then ["src"] else args)

extensions :: [String]
extensions =
  [ "DataKinds"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveTraversable"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "MultiParamTypeClasses"
  , "OverloadedStrings"
  , "RecordWildCards"
  , "StandaloneDeriving"
  , "StrictData"
  , "TypeApplications"
  ]
