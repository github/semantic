{-# LANGUAGE OverloadedStrings #-}
module Numeric.Spec
  ( testTree
  ) where

import           Data.Either
import           Data.Text (Text)
import           Data.Foldable (traverse_)
import           Numeric.Exts
import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit

type Fixture = (Text, Integer)

python :: [Fixture]
python = [ ("-1", (negate 1))
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

ruby :: [Fixture]
ruby = [ ("0xa_bcd_ef0_123_456_789", 0xabcdef0123456789)
       , ("01234567", 342391)
       ]

testTree :: Tasty.TestTree
testTree =
  let go = traverse_ (\(s, v) -> parseInteger s @?= Right v)
  in Tasty.testGroup "Numeric.Exts"
     [ testCase "handles Python integers" (go python)
     , testCase "handles Ruby integers" (go ruby)
     , testCase "doesn't accept floats" (isLeft (parseInteger "1.5") @? "Accepted floating-point")
     , testCase "doesn't accept empty string" (isLeft (parseInteger "") @? "Accepted integer")
     ]
