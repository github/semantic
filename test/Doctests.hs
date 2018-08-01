module Main
( main
) where

import System.Environment
import Test.DocTest

defaultFiles =
  [ "src/Data/Abstract/Address.hs"
  , "src/Data/Abstract/Environment.hs"
  , "src/Data/Abstract/Name.hs"
  , "src/Data/Graph.hs"
  , "src/Data/Range.hs"
  , "src/Data/Semigroup/App.hs"
  ]

main :: IO ()
main = do
  args <- getArgs
  doctest (map ("-X" ++) extensions ++ "-isrc" : "--fast" : if null args then defaultFiles else args)

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
