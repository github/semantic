{-# LANGUAGE OverloadedStrings #-}
module Generators
  ( source
  , integerScientific
  , rationalScientific
  , floatingScientific
  , classifyScientific
  ) where

import           Data.Ratio ((%))
import           Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Source.Source


source :: MonadGen m => Hedgehog.Range Int -> m Source.Source.Source
source r = Gen.frequency [ (1, empty), (20, nonEmpty) ]
  where empty    = pure mempty
        nonEmpty = Source.Source.fromUTF8 <$> Gen.utf8 r (Gen.frequency [ (1, pure '\r'), (1, pure '\n'), (20, Gen.unicode) ])

integerScientific :: MonadGen m => Hedgehog.Range Integer -> m Scientific
integerScientific = fmap fromIntegral . Gen.integral

rationalScientific :: MonadGen m => Hedgehog.Range Integer -> Hedgehog.Range Integer -> m Scientific
rationalScientific nrange drange = do
  num <- Gen.integral nrange
  den <- Gen.integral drange
  let goodDen = if den == 0 then 1 else den
  let digitLimit = Just 25
  case Scientific.fromRationalRepetend digitLimit (num % goodDen) of
    Left (sci, _)  -> pure sci
    Right (sci, _) -> pure sci

floatingScientific :: MonadGen m => Hedgehog.Range Double -> m Scientific
floatingScientific = fmap Scientific.fromFloatDigits . Gen.double

classifyScientific :: MonadTest m => Scientific -> m ()
classifyScientific sci = do
  classify "negative" $ sci < 0
  classify "small" $ (sci > 0 && sci <= 1)
  classify "medium" $ (sci > 1 && sci <= 10000)
  classify "large" $ sci > 10000
