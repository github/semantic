{-# LANGUAGE OverloadedStrings #-}
module Data.Scientific.Spec (testTree) where

import           Data.Either
import           Data.Foldable (for_)
import           Data.Scientific.Exts
import           Data.Text as Text
import qualified Generators as Gen
import           Hedgehog
import qualified Hedgehog.Range as Range
import           Test.Tasty as Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit as HUnit

type Fixture = [(Text, Scientific)]

testFixture :: [(Text, Scientific)] -> HUnit.Assertion
testFixture vals = for_ vals $ \(input, expected) -> assertEqual (Text.unpack input) (parseScientific input) (Right expected)

pythonSyntax :: Fixture
pythonSyntax =  [ ("-.6_6", -0.66)
                , ("+.1_1", 0.11)
                , ("123.4123", 123.4123)
                , ("123.123J", 123.123) -- TODO: handle complex values separately in the parser
                , ("1_1.3_1", 11.31)
                , ("1_1.", 11.0)
                , ("99E+01", 99e1)
                , ("1e+3_4j", 1e34)
                , ("3.e14", 3e14)
                , (".3e1_4", 0.3e14)
                , ("1_0.l", 10) -- this and the subsequent ones don't actually seem to be valid syntax, we should fix this in tree-sitter
                , (".3", 0.3)
                , (".1l", 0.1)  -- omitting a leading 0 is deprecated in python 3, also note that the -l suffix is not valid in Python 3
                ]

rubySyntax :: Fixture
rubySyntax = [ ("1.234_5e1_0", 1.2345e10)
             , ("1E30", 1e30)
             , ("1.2i", 1.2)
             , ("1.0e+6", 1.0e6)
             , ("1.0e-6", 1.0e-6)
             ]

jsSyntax :: Fixture
jsSyntax = [ ("101", 101)
           , ("3.14", 3.14)
           , ("3.14e+1", 3.14e1)
           , ("0x1ABCDEFabcdef", 470375954370031)
           , ("0o7632157312", 1047060170)
           , ("0b1010101001", 681)
           ]

testTree :: Tasty.TestTree
testTree = testGroup "Data.Scientific.Exts"
  [ testCase "Python float syntax" $ testFixture pythonSyntax
  , testCase "Ruby float syntax" $ testFixture rubySyntax
  , testCase "JavaScript float syntax" $ testFixture jsSyntax
  , testCase "Pathological input" $ do
      isLeft (parseScientific ".") @? "Accepted period"
      isLeft (parseScientific "")  @? "Accepted empty string"
  , testProperty "Scientific roundtripping" . property $ do
      let nrange = Range.linear (negate 500000) 20000000
          drange = Range.exponential 1 100000000
      fromRat <- forAll (Gen.rationalScientific nrange drange)
      Gen.classifyScientific fromRat
      tripping fromRat (pack . show) parseScientific
  , testProperty "Double-based Scientific roundtripping" . property $ do
      fromDbl <- forAll (Gen.floatingScientific (Range.linearFrac (negate 1) 3))
      Gen.classifyScientific fromDbl
      tripping fromDbl (pack . show) parseScientific
  ]
