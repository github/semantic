module Main
( main
) where

import qualified Source.Test as Source
import           Test.Tasty as Tasty

main :: IO ()
main = defaultMain $ testGroup "semantic-source"
  [ Source.testTree
  ]
