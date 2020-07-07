{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import Tags.Tagging.Precise
import Data.Text (Text)
import Source.Source as Source
import Source.Loc
import Source.Span
import Test.Tasty as Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "semantic-tags" [ testTree ]

src :: Text -> Source
src = Source.fromText

-- | For testing
calculateLineAndSpans' :: Source -> Loc -> (Text, OneIndexedSpan, UTF16CodeUnitSpan)
calculateLineAndSpans' src loc = let (a, b, c, _) = calculateLineAndSpans src (LineIndices mempty) loc in (a, b, c)

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Tags.Tagging.Precise"
  [ Tasty.testGroup "countUtf16CodeUnits from utf8 encoded bytestring"
    [ testCase "one byte" $ do
        1 @=? countUtf16CodeUnits (src "a")

    , testCase "null byte" $ do
        1 @=? countUtf16CodeUnits (src "\0") -- NULL

    , testCase "two bytes" $ do
        1 @=? countUtf16CodeUnits (src "\x0080") -- ""
        1 @=? countUtf16CodeUnits (src "à")

    , testCase "three bytes" $ do
        1 @=? countUtf16CodeUnits (src "\x0800") -- "ࠀ"

    , testCase "four bytes" $ do
        2 @=? countUtf16CodeUnits (src "𐀀")
        2 @=? countUtf16CodeUnits (src "😀")

    , testCase "multibyte" $ do
        4 @=? countUtf16CodeUnits (src "👋🏻")
    ]

    -- Range start = 13 + *num bytes in char inside ''*
    -- Span start col = 5 + *num bytes in char*
  , Tasty.testGroup "firstLineAndSpans"
    [ testCase "one line" $
        let src = Source.fromText "def foo;end"
            loc = Loc (Range 4 7) (Span (Pos 0 4) (Pos 0 7))
        in ( "def foo;end"
           , OneIndexedSpan $ Span (Pos 1 5) (Pos 1 8) -- one indexed, counting bytes
           , UTF16CodeUnitSpan $ Span (Pos 0 4) (Pos 0 7) -- zero-indexed, counting utf16 code units (lsp-style column offset)
           ) @=? calculateLineAndSpans' src loc

    , testCase "ascii" $
        let src = Source.fromText "def foo\n  'a'.hi\nend\n"
            loc = Loc (Range 14 16) (Span (Pos 1 6) (Pos 1 8))
        in ( "'a'.hi"
           , OneIndexedSpan $ Span (Pos 2 7) (Pos 2 9) -- one indexed, counting bytes
           , UTF16CodeUnitSpan $ Span (Pos 1 6) (Pos 1 8) -- zero-indexed, counting utf16 code units (lsp-style column offset)
           ) @=? calculateLineAndSpans' src loc

    , testCase "unicode" $
        let src = Source.fromText "def foo\n  'à'.hi\nend\n"
            loc = Loc (Range 15 17) (Span (Pos 1 7) (Pos 1 9))
        in ( "'à'.hi"
           , OneIndexedSpan $ Span (Pos 2 8) (Pos 2 10) -- one indexed, counting bytes
           , UTF16CodeUnitSpan $ Span (Pos 1 6) (Pos 1 8)  -- zero-indexed, counting utf16 code units (lsp-style column offset)
           ) @=? calculateLineAndSpans' src loc

    , testCase "multi code point unicode" $
        let src = Source.fromText "def foo\n  '💀'.hi\nend\n"
            loc = Loc (Range 17 19) (Span (Pos 1 9) (Pos 1 11))
        in ( "'💀'.hi"
           , OneIndexedSpan $ Span (Pos 2 10) (Pos 2 12) -- one indexed, counting bytes
           , UTF16CodeUnitSpan $ Span (Pos 1 7) (Pos 1 9)   -- zero-indexed, counting utf16 code units (lsp-style column offset)
           ) @=? calculateLineAndSpans' src loc

    -- NB: This emoji (:man-woman-girl-girl:) cannot be entered into a string literal in haskell for some reason, you'll get:
    --   > lexical error in string/character literal at character '\8205'
    -- The work around is to enter the unicode directly (7 code points).
    -- utf-8: 25 bytes to represent
    -- utf-16: 23 bytes to represent
    , testCase "multi code point unicode :man-woman-girl-girl:" $
        let src = Source.fromText "def foo\n  '\128104\8205\128105\8205\128103\8205\128103'.hi\nend\n"
            loc = Loc (Range 38 40) (Span (Pos 1 30) (Pos 1 32))
        in ( "'\128104\8205\128105\8205\128103\8205\128103'.hi"
           , OneIndexedSpan $ Span (Pos 2 31) (Pos 2 33)  -- one indexed, counting bytes
           , UTF16CodeUnitSpan $ Span (Pos 1 16) (Pos 1 18)  -- zero-indexed, counting utf16 code units (lsp-style column offset)
           ) @=? calculateLineAndSpans' src loc
    ]
  , Tasty.testGroup "slice180"
    [ testCase "shorter than 180 chars" $ do
        "def foo" @=? slice180 (Pos 0 0) "def foo"

    , testCase "180 chars at position 0" $ do
        line180chars @=? slice180 (Pos 0 0) line180chars

    , testCase "180 chars at position 179" $ do
        line180chars @=? slice180 (Pos 0 179) line180chars

    , testCase "240 chars at position 0" $ do
        line180chars
            @=? slice180 (Pos 0 0) line240chars

    , testCase "240 chars at position 179" $ do
        line180chars
            @=? slice180 (Pos 0 179) line240chars

    , testCase "240 chars at position 180" $ do
        line180chars
            @=? slice180 (Pos 0 180) line240chars

    , testCase "240 chars at position 182" $ do
        "unction(e,t){\"use strict\";\"object\"==typeof module&&\"object\"==typeof module.exports?module.exports=e.document?t(e,!0):function(e){if(!e.document)throw new Error(\"jQuery requires a w"
            @=? slice180 (Pos 0 182) line240chars
    ]
  ]

  where
    line180chars = "!function(e,t){\"use strict\";\"object\"==typeof module&&\"object\"==typeof module.exports?module.exports=e.document?t(e,!0):function(e){if(!e.document)throw new Error(\"jQuery requires a"
    line240chars = "!function(e,t){\"use strict\";\"object\"==typeof module&&\"object\"==typeof module.exports?module.exports=e.document?t(e,!0):function(e){if(!e.document)throw new Error(\"jQuery requires a window with a document\");return t(e)}:t(e)}(\"undefined\"!=ty"
