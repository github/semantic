{-# LANGUAGE TemplateHaskell #-}

module Numeric.Spec
  ( spec
  ) where

import Data.Either
import Data.Text (Text)
import Data.Foldable
import Numeric.Exts

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

spec :: TestTree
spec = $(testGroupGenerator)

assertParses :: [(Text, Integer)] -> Assertion
assertParses = traverse_ (\(s, v) -> parseInteger s @?= Right v)

case_should_handle_Python_integers :: Assertion
case_should_handle_Python_integers =
  assertParses [ ("-1", (negate 1))
               , ("0xDEAD", 0xDEAD)
               , ("0XDEAD", 0xDEAD)
               , ("1j", 1)
               , ("0o123", 83)
               , ("0O123", 83)
               , ("0b001", 1)
               , ("0B001", 1)
               , ("1_1", 11) -- underscore syntax is Python 3 only
               , ("0B1_1", 3)
               , ("0O1_1", 9)
               , ("0L", 0)
               ]

case_should_handle_Ruby_integers :: Assertion
case_should_handle_Ruby_integers =
  assertParses [ ("0xa_bcd_ef0_123_456_789", 0xabcdef0123456789)
               , ("01234567", 342391)
               ]

case_should_not_accept_floating_points :: Assertion
case_should_not_accept_floating_points =
  isLeft (parseInteger "1.5") @? "Incorrectly accepted a float"


case_should_not_accept_the_empty_string :: Assertion
case_should_not_accept_the_empty_string =
  isLeft (parseInteger "") @? "Incorrectly accepted the empty string"
