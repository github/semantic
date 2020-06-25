{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import Tags.Tagging.Precise
import Source.Source as Source
import Source.Loc
import Source.Span
import Test.Tasty as Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "semantic-tags" [ testTree ]

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Tags.Tagging.Precise"
  [ Tasty.testGroup "utf16CodeUnitsSpan"
    [ testCase "ascii" $
        ( "  'a'.hi"
        , Span (Pos 2 7) (Pos 2 9)  -- one indexed, counting bytes
        , Span (Pos 1 6) (Pos 1 8)) -- zero-indexed, counting utf16 code units (lsp-style column offset)
        @=?
          utf16CodeUnitsSpan (Source.fromText "def foo\n  'a'.hi\nend\n") (Loc (Range 14 16) (Span (Pos 1 6) (Pos 1 8)))
          --                            0123456 789
          --                                       ^10
          --                                        ^11
          --                                         ^12
    , testCase "unicode" $
        ( "  'à'.hi"
        , Span (Pos 2 8) (Pos 2 10) -- one indexed, counting bytes
        , Span (Pos 1 6) (Pos 1 8)) -- zero-indexed, counting utf16 code units (lsp-style column offset)
        @=?
          utf16CodeUnitsSpan (Source.fromText "def foo\n  'à'.hi\nend\n") (Loc (Range 15 17) (Span (Pos 1 7) (Pos 1 9)))
          --                            0123456 789
          --                                 10 ~~~^
          --                             11, 12 ~~~~^
          --                                 13 ~~~~~^
    , testCase "multi code point unicode" $
        ( "  '💀'.hi"
        , Span (Pos 2 10) (Pos 2 12) -- one indexed, counting bytes
        , Span (Pos 1 7) (Pos 1 9)) -- zero-indexed, counting utf16 code units (lsp-style column offset)
        @=?
          utf16CodeUnitsSpan (Source.fromText "def foo\n  '💀'.hi\nend\n") (Loc (Range 17 19) (Span (Pos 1 9) (Pos 1 11)))
          --                            0123456 789
          --                                 10 ~~~^
          --                             11, 12 ~~~~^
          --                                 13 ~~~~~^
    ]
  ]
