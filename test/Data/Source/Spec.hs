module Data.Source.Spec (spec) where

import Data.Char (chr)
import Data.Functor.Listable
import Data.Range
import Data.Semigroup
import Data.Source
import Data.Span
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "sourceLineRanges" $ do
    prop "produces 1 more range than there are newlines" $
      \ source -> length (sourceLineRanges source) `shouldBe` succ (Text.count "\n" (toText source))

    prop "produces exhaustive ranges" $
      \ source -> foldMap (`slice` source) (sourceLineRanges source) `shouldBe` source

  describe "spanToRange" $ do
    prop "computes single-line ranges" $
      \ s -> let source = fromUTF8 s
                 spans = zipWith (\ i Range {..} -> Span (Pos i 1) (Pos i (succ (end - start)))) [1..] ranges
                 ranges = sourceLineRanges source in
        spanToRange source <$> spans `shouldBe` ranges

    prop "computes multi-line ranges" $
      \ source ->
        spanToRange source (totalSpan source) `shouldBe` totalRange source

    prop "computes sub-line ranges" $
      \ s -> let source = "*" <> s <> "*" in
        spanToRange source (insetSpan (totalSpan source)) `shouldBe` insetRange (totalRange source)

    prop "inverse of rangeToSpan" $
      \ a b -> let s = a <> "\n" <> b in spanToRange s (totalSpan s) `shouldBe` totalRange s

  describe "rangeToSpan" $ do
    prop "inverse of spanToRange" $
      \ a b -> let s = a <> "\n" <> b in rangeToSpan s (totalRange s) `shouldBe` totalSpan s

  describe "totalSpan" $ do
    prop "covers single lines" $
      \ n -> totalSpan (fromText (Text.replicate n "*")) `shouldBe` Span (Pos 1 1) (Pos 1 (max 1 (succ n)))

    prop "covers multiple lines" $
      \ n -> totalSpan (fromText (Text.intersperse '\n' (Text.replicate n "*"))) `shouldBe` Span (Pos 1 1) (Pos (max 1 n) (if n > 0 then 2 else 1))

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

  prop "preserves characters" . forAll (toTiers (list +| [chr 0xa0..chr 0x24f])) $
    \ c -> Text.unpack (toText (fromText (Text.singleton c))) `shouldBe` [c]

  prop "preserves strings" $
    \ s -> fromText (toText s) `shouldBe` s


insetSpan :: Span -> Span
insetSpan sourceSpan = sourceSpan { spanStart = (spanStart sourceSpan) { posColumn = succ (posColumn (spanStart sourceSpan)) }
                                  , spanEnd = (spanEnd sourceSpan) { posColumn = pred (posColumn (spanEnd sourceSpan)) } }

insetRange :: Range -> Range
insetRange Range {..} = Range (succ start) (pred end)
