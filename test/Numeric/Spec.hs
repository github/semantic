module Numeric.Spec
  ( spec
  ) where

import SpecHelpers
import Data.Either
import Numeric.Exts

spec :: Spec
spec = describe "Integer parsing" $ do

  let go cases = forM_ cases $ \(s, v) -> parseInteger s `shouldBe` Right v

  it "should handle Python integers" $
    go [ ("-1", (negate 1))
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

  it "should handle Ruby integers" $
    go [ ("0xa_bcd_ef0_123_456_789", 0xabcdef0123456789)
       , ("01234567", 342391)
       ]

  it "should not accept floating-points" $ do
    parseInteger "1.5" `shouldSatisfy` isLeft

  it "should not accept the empty string" $ do
    parseInteger "" `shouldSatisfy` isLeft
