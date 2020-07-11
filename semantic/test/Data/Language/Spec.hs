module Data.Language.Spec (testTree) where

import           Source.Language as Language
import qualified System.Path as Path
import           Test.Tasty
import           Test.Tasty.HUnit

testTree :: TestTree
testTree = testGroup "Data.Language"
  [ testCase "languageForFilePath works for languages with ambiguous lingo extensions" $ do
      Language.forPath (Path.relFile "foo.php") @=? PHP
      Language.forPath (Path.relFile "foo.md" ) @=? Markdown
      Language.forPath (Path.relFile "foo.tsx") @=? TSX
  ]
