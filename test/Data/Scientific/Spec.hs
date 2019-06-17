module Data.Scientific.Spec (spec) where

import Data.Scientific.Exts
import Data.Either
import Data.Foldable (for_)
import SpecHelpers

spec :: Spec
spec = describe "Scientific parsing" $ do

  let go cases = for_ cases $ \(s, v) -> parseScientific s `shouldBe` Right v

  -- TODO: hexadecimal floats, someday (0x1.999999999999ap-4)

  it "should handle Python floats" $
    go [ ("-.6_6", -0.66)
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

  it "should handle Ruby floats" $
    go [ ("1.234_5e1_0", 1.2345e10)
       , ("1E30", 1e30)
       , ("1.2i", 1.2)
       , ("1.0e+6", 1.0e6)
       , ("1.0e-6", 1.0e-6)
       ]

  it "should handle JS numbers, including multiple bases" $
    go [ ("101", 101)
       , ("3.14", 3.14)
       , ("3.14e+1", 3.14e1)
       , ("0x1ABCDEFabcdef", 470375954370031)
       , ("0o7632157312", 1047060170)
       , ("0b1010101001", 681)
       ]

  it "should not accept truly bad input" $ do
    parseScientific "." `shouldSatisfy` isLeft
    parseScientific ""  `shouldSatisfy` isLeft
