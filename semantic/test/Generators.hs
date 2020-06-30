{-# LANGUAGE OverloadedStrings #-}
module Generators
  ( source
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Source.Source

source :: MonadGen m => Hedgehog.Range Int -> m Source.Source.Source
source r = Gen.frequency [ (1, empty), (20, nonEmpty) ]
  where empty    = pure mempty
        nonEmpty = Source.Source.fromUTF8 <$> Gen.utf8 r (Gen.frequency [ (1, pure '\r'), (1, pure '\n'), (20, Gen.unicode) ])
