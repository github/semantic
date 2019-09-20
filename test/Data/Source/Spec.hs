module Data.Source.Spec (spec, testTree) where

import qualified Data.Text as Text
import           Source.Range
import           Source.Source (Source)
import qualified Source.Source as Source
import           Source.Span

import Test.Hspec

import qualified Generators as Gen
import           Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range
import qualified Test.Tasty as Tasty
import           Test.Tasty.Hedgehog (testProperty)

prop :: HasCallStack => String -> (Source -> PropertyT IO ()) -> Tasty.TestTree
prop desc f
  = testProperty desc
  . property
  $ forAll (Gen.source (Hedgehog.Range.linear 0 100))
  >>= f

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Data.Source"
  [ Tasty.testGroup "sourceLineRanges"
    [ prop "produces 1 more range than there are newlines" $ \ source -> do
        summarize source
        length (Source.lineRanges source) === length (Text.splitOn "\r\n" (Source.toText source) >>= Text.splitOn "\r" >>= Text.splitOn "\n")

    , prop "produces exhaustive ranges" $ \ source -> do
        summarize source
        foldMap (Source.slice source) (Source.lineRanges source) === source
    ]

  , Tasty.testGroup "totalSpan"
    [ testProperty "covers single lines" . property $ do
        n <- forAll $ Gen.int (Hedgehog.Range.linear 0 100)
        Source.totalSpan (Source.fromText (Text.replicate n "*")) === Span (Pos 1 1) (Pos 1 (max 1 (succ n)))

    , testProperty "covers multiple lines" . property $ do
        n <- forAll $ Gen.int (Hedgehog.Range.linear 0 100)
        Source.totalSpan (Source.fromText (Text.intersperse '\n' (Text.replicate n "*"))) === Span (Pos 1 1) (Pos (max 1 n) (if n > 0 then 2 else 1))
    ]

  ]
  where summarize src = do
          let lines = Source.lines src
          -- FIXME: this should be using cover (reverted in 1b427b995), but that leads to flaky tests: hedgehog’s 'cover' implementation fails tests instead of warning, and currently has no equivalent to 'checkCoverage'.
          classify "empty"          $ Source.null src
          classify "single-line"    $ length lines == 1
          classify "multiple lines" $ length lines >  1

spec :: Spec
spec = do
  describe "newlineIndices" $ do
    it "finds \\n" $
      let source = "a\nb" in
      Source.newlineIndices source `shouldBe` [1]
    it "finds \\r" $
      let source = "a\rb" in
      Source.newlineIndices source `shouldBe` [1]
    it "finds \\r\\n" $
      let source = "a\r\nb" in
      Source.newlineIndices source `shouldBe` [2]
    it "finds intermixed line endings" $
      let source = "hi\r}\r}\n xxx \r a" in
      Source.newlineIndices source `shouldBe` [2, 4, 6, 12]

insetSpan :: Span -> Span
insetSpan sourceSpan = sourceSpan { spanStart = (spanStart sourceSpan) { posColumn = succ (posColumn (spanStart sourceSpan)) }
                                  , spanEnd = (spanEnd sourceSpan) { posColumn = pred (posColumn (spanEnd sourceSpan)) } }

insetRange :: Range -> Range
insetRange Range {..} = Range (succ start) (pred end)
