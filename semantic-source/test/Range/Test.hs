module Range.Test
( testTree
) where

import qualified Test.Tasty as Tasty

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Source.Range"
  []
