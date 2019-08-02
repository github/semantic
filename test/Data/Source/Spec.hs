{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Source.Spec (spec, testTree) where

import Data.Range
import Data.Source
import Data.Span
import qualified Data.Text as Text

import Test.Hspec

import qualified Generators as Gen
import qualified Hedgehog.Gen as Gen
import           Hedgehog ((===))
import qualified Hedgehog.Range
import           Hedgehog hiding (Range)
import qualified Test.Tasty as Tasty
import           Test.Tasty.Hedgehog (testProperty)
import qualified Test.Tasty.QuickCheck as QC

prop :: HasCallStack => String -> (Source -> PropertyT IO ()) -> Tasty.TestTree
prop desc f
  = testProperty desc
  . property
  $ forAll (Gen.source (Hedgehog.Range.linear 0 100))
  >>= f

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Data.Source"
  [ Tasty.testGroup "sourceLineRanges"
    [ QC.testProperty "produces 1 more range than there are newlines" $
      \ source -> length (sourceLineRanges source) QC.=== succ (Text.count "\n" (toText source))

    , prop "produces exhaustive ranges" $
      \ source -> foldMap (`slice` source) (sourceLineRanges source) === source
    ]

  , Tasty.testGroup "spanToRange"
    [ prop "computes single-line ranges" $ \ source -> do
        let ranges = sourceLineRanges source
        let spans = zipWith (\ i Range {..} -> Span (Pos i 1) (Pos i (succ (end - start)))) [1..] ranges
        fmap (spanToRange source) spans === ranges

    , prop "computes multi-line ranges" $
        \ source ->
          spanToRange source (totalSpan source) === totalRange source

    , prop "computes sub-line ranges" $
        \ s -> let source = "*" <> s <> "*" in
          spanToRange source (insetSpan (totalSpan source)) === insetRange (totalRange source)

    , testProperty "inverse of rangeToSpan" . property $ do
        a <- forAll . Gen.source $ Hedgehog.Range.linear 0 100
        b <- forAll . Gen.source $ Hedgehog.Range.linear 0 100
        let s = a <> "\n" <> b in spanToRange s (totalSpan s) === totalRange s
    ]

  ,  testProperty "rangeToSpan inverse of spanToRange" . property $ do
      a <- forAll . Gen.source $ Hedgehog.Range.linear 0 100
      b <- forAll . Gen.source $ Hedgehog.Range.linear 0 100
      let s = a <> "\n" <> b in rangeToSpan s (totalRange s) === totalSpan s

  , Tasty.testGroup "totalSpan"
    [ testProperty "covers single lines" . property $ do
        n <- forAll $ Gen.int (Hedgehog.Range.linear 0 100)
        totalSpan (fromText (Text.replicate n "*")) === Span (Pos 1 1) (Pos 1 (max 1 (succ n)))

    , testProperty "covers multiple lines" . property $ do
        n <- forAll $ Gen.int (Hedgehog.Range.linear 0 100)
        totalSpan (fromText (Text.intersperse '\n' (Text.replicate n "*"))) === Span (Pos 1 1) (Pos (max 1 n) (if n > 0 then 2 else 1))
    ]

  ]

spec :: Spec
spec = do
  describe "newlineIndices" $ do
    it "finds \\n" $
      let source = "a\nb" in
      newlineIndices source `shouldBe` [1]
    it "finds \\r" $
      let source = "a\rb" in
      newlineIndices source `shouldBe` [1]
    it "finds \\r\\n" $
      let source = "a\r\nb" in
      newlineIndices source `shouldBe` [2]
    it "finds intermixed line endings" $
      let source = "hi\r}\r}\n xxx \r a" in
      newlineIndices source `shouldBe` [2, 4, 6, 12]

insetSpan :: Span -> Span
insetSpan sourceSpan = sourceSpan { spanStart = (spanStart sourceSpan) { posColumn = succ (posColumn (spanStart sourceSpan)) }
                                  , spanEnd = (spanEnd sourceSpan) { posColumn = pred (posColumn (spanEnd sourceSpan)) } }

insetRange :: Range -> Range
insetRange Range {..} = Range (succ start) (pred end)


instance QC.Arbitrary Source where
  arbitrary = fromText . Text.pack <$> QC.listOf (QC.oneof [ pure '\r' , pure '\n' , QC.arbitraryUnicodeChar ])
  shrink src = fromText . Text.pack <$> QC.shrinkList QC.shrinkNothing (Text.unpack (toText src))
