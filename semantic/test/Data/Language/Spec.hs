module Data.Language.Spec (testTree) where

import Source.Language as Language
import Test.Tasty
import Test.Tasty.HUnit

testTree :: TestTree
testTree = testGroup "Data.Language"
  [ testCase "languageForFilePath works for languages with ambiguous lingo extensions" $ do
      Language.forPath "foo.php" @=? PHP
      Language.forPath "foo.md"  @=? Markdown
      Language.forPath "foo.tsx" @=? TSX
  ]
