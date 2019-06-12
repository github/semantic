module Data.Source.Spec (spec) where

import Data.Char (chr)
import Data.Functor.Listable
import Data.Range
import Data.Source
import Data.Span
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck
import Test.LeanCheck


spec :: TestTree
spec = testGroup "Data.Source.Spec"
  [ testGroup "sourceLineRanges" $
    [ testProperty "produces 1 more range than there are newlines" $
        \ source -> length (sourceLineRanges source) == succ (Text.count "\n" (toText source))

    , testProperty "produces exhaustive ranges" $
        \ source -> foldMap (`slice` source) (sourceLineRanges source) == source
    ]

  , testGroup "spanToRange"
    [ testProperty "computes single-line ranges" $
      \ s -> let source = fromUTF8 s
                 spans = zipWith (\ i Range {..} -> Span (Pos i 1) (Pos i (succ (end - start)))) [1..] ranges
                 ranges = sourceLineRanges source in
        (spanToRange source <$> spans) == ranges

    , testProperty "computes multi-line ranges" $
        \ source ->
          spanToRange source (totalSpan source) == totalRange source

    , testProperty "computes sub-line ranges" $
        \ s -> let source = "*" <> s <> "*" in
          spanToRange source (insetSpan (totalSpan source)) == insetRange (totalRange source)

    , testProperty "inverse of rangeToSpan" $
        \ a b -> let s = a <> "\n" <> b in spanToRange s (totalSpan s) == totalRange s

    ]

  , testProperty "rangeToSpan is the inverse of spanToRange" $
      \ a b -> let s = a <> "\n" <> b in rangeToSpan s (totalRange s) == totalSpan s

  , testGroup "totalSpan"
    [ testProperty "covers single lines" $
      \ n -> totalSpan (fromText (Text.replicate n "*")) == Span (Pos 1 1) (Pos 1 (max 1 (succ n)))

    , testProperty "covers multiple lines" $
      \ n -> totalSpan (fromText (Text.intersperse '\n' (Text.replicate n "*"))) == Span (Pos 1 1) (Pos (max 1 n) (if n > 0 then 2 else 1))
    ]

  , testGroup "newlineIndices" $
    [ testCase "finds \\n" $
        let source = "a\nb" in
        newlineIndices source @?= [1]
    , testCase "finds \\r" $
        let source = "a\rb" in
        newlineIndices source @?= [1]
    , testCase "finds \\r\\n" $
      let source = "a\r\nb" in
      newlineIndices source @?= [2]
    , testCase "finds intermixed line endings" $
      let source = "hi\r}\r}\n xxx \r a" in
      newlineIndices source @?= [2, 4, 6, 12]
    ]

  , testProperty "preserves strings" $
    \ s -> fromText (toText s) == s
  ]


insetSpan :: Span -> Span
insetSpan sourceSpan = sourceSpan { spanStart = (spanStart sourceSpan) { posColumn = succ (posColumn (spanStart sourceSpan)) }
                                  , spanEnd = (spanEnd sourceSpan) { posColumn = pred (posColumn (spanEnd sourceSpan)) } }

insetRange :: Range -> Range
insetRange Range {..} = Range (succ start) (pred end)
