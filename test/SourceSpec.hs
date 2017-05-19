module SourceSpec where

import qualified Data.Text as Text
import qualified Prelude
import Prologue hiding (list)
import Range
import Source
import SourceSpan
import Test.Hspec
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "actualLineRanges" $ do
    prop "produces 1 more range than there are newlines" $
      \ source -> Prologue.length (actualLineRanges source) `shouldBe` succ (Text.count "\n" (toText source))

    prop "produces exhaustive ranges" $
      \ source -> foldMap (`slice` source) (actualLineRanges source) `shouldBe` source

  describe "sourceSpanToRange" $ do
    prop "computes single-line ranges" . forAll (unListableByteString `mapT` tiers) $
      \ s -> let source = Source s
                 spans = zipWith (\ i Range {..} -> SourceSpan (SourcePos i 1) (SourcePos i (succ (end - start)))) [1..] ranges
                 ranges = actualLineRanges source in
        sourceSpanToRange source <$> spans `shouldBe` ranges

    prop "computes multi-line ranges" $
      \ source ->
        sourceSpanToRange source (totalSpan source) `shouldBe` totalRange source

    prop "computes sub-line ranges" $
      \ s -> let source = "*" <> s <> "*" in
        sourceSpanToRange source (insetSpan (totalSpan source)) `shouldBe` insetRange (totalRange source)

    prop "inverse of rangeToSourceSpan" $
      \ s -> sourceSpanToRange s (totalSpan s) `shouldBe` totalRange s

  describe "rangeToSourceSpan" $ do
    prop "inverse of sourceSpanToRange" $
      \ s -> rangeToSourceSpan s (totalRange s) `shouldBe` totalSpan s

  describe "totalSpan" $ do
    prop "covers single lines" $
      \ n -> totalSpan (fromText (Text.replicate n "*")) `shouldBe` SourceSpan (SourcePos 1 1) (SourcePos 1 (max 1 (succ n)))

    prop "covers multiple lines" $
      \ n -> totalSpan (fromText (Text.intersperse '\n' (Text.replicate n "*"))) `shouldBe` SourceSpan (SourcePos 1 1) (SourcePos (max 1 n) (if n > 0 then 2 else 1))

  prop "preserves characters" . forAll (toTiers (list +| [chr 0xa0..chr 0x24f])) $
    \ c -> Text.unpack (toText (fromText (Text.singleton c))) `shouldBe` [c]

  prop "preserves strings" $
    \ s -> fromText (toText s) `shouldBe` s


insetSpan :: SourceSpan -> SourceSpan
insetSpan sourceSpan = sourceSpan { spanStart = (spanStart sourceSpan) { column = succ (column (spanStart sourceSpan)) }
                                  , spanEnd = (spanEnd sourceSpan) { column = pred (column (spanEnd sourceSpan)) } }

insetRange :: Range -> Range
insetRange Range {..} = Range (succ start) (pred end)
