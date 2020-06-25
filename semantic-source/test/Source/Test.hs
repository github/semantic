{-# LANGUAGE OverloadedStrings #-}
module Source.Test
( testTree
) where

import qualified Data.Text as Text
import           Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Source.Source as Source
import           Source.Span
import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog (testProperty)


source :: MonadGen m => Range.Range Int -> m Source.Source
source r = Gen.frequency [ (1, empty), (20, nonEmpty) ] where
  empty    = pure mempty
  nonEmpty = Source.fromUTF8 <$> Gen.utf8 r (Gen.frequency [ (1, pure '\r'), (1, pure '\n'), (20, Gen.unicode) ])

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Data.Source"
  [ Tasty.testGroup "lineRanges"
    [ testProperty "produces 1 more range than there are newlines" . property $ do
        source <- forAll (source (Range.linear 0 100))
        summarize source
        length (Source.lineRanges source) === length (Text.splitOn "\r\n" (Source.toText source) >>= Text.splitOn "\r" >>= Text.splitOn "\n")

    , testProperty "produces exhaustive ranges" . property $ do
        source <- forAll (source (Range.linear 0 100))
        summarize source
        foldMap (Source.slice source) (Source.lineRanges source) === source
    ]

  , Tasty.testGroup "totalSpan"
    [ testProperty "covers single lines" . property $ do
        n <- forAll $ Gen.int (Range.linear 0 100)
        Source.totalSpan (Source.fromText (Text.replicate n "*")) === Span (Pos 1 1) (Pos 1 (max 1 (succ n)))

    , testProperty "covers multiple lines" . property $ do
        n <- forAll $ Gen.int (Range.linear 0 100)
        Source.totalSpan (Source.fromText (Text.intersperse '\n' (Text.replicate n "*"))) === Span (Pos 1 1) (Pos (max 1 n) (if n > 0 then 2 else 1))
    ]

  , Tasty.testGroup "newlineIndices"
    [ testCase "finds \\n" $ Source.newlineIndices "a\nb" @?= [1]
    , testCase "finds \\r" $ Source.newlineIndices "a\rb" @?= [1]
    , testCase "finds \\r\\n" $ Source.newlineIndices "a\r\nb" @?= [2]
    , testCase "finds intermixed line endings" $ Source.newlineIndices "hi\r}\r}\n xxx \r a" @?= [2, 4, 6, 12]
    ]
  ]

summarize :: Source.Source -> PropertyT IO ()
summarize src = do
  let lines = Source.lines src
  -- FIXME: this should be using cover (reverted in 1b427b995), but that leads to flaky tests: hedgehog’s 'cover' implementation fails tests instead of warning, and currently has no equivalent to 'checkCoverage'.
  classify "empty"          $ Source.null src
  classify "single-line"    $ length lines == 1
  classify "multiple lines" $ length lines >  1
