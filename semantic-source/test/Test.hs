module Main
( main
) where

import qualified Range.Test as Range
import qualified Source.Test as Source
import           Test.Tasty as Tasty

main :: IO ()
main = defaultMain $ testGroup "semantic-source"
  [ Range.testTree
  , Source.testTree
  ]
