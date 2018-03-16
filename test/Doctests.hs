module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = doctest (map ("-X" ++) extensions ++ args ++ ["-isrc"] ++ sources)

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

sources :: [String]
sources =
  [ "src/Data/Abstract/Environment.hs"
  ]
