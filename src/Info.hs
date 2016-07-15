{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module Info where

import Data.Record
import Prologue
import Category
import Range
import Test.QuickCheck

newtype Size = Size { unSize :: Integer }
  deriving (Eq, Num, Ord, Show)
newtype Cost = Cost { unCost :: Integer }
  deriving (Eq, Num, Ord, Show)

characterRange :: HasField fields Range => Record fields -> Range
characterRange = getField

setCharacterRange :: HasField fields Range => Record fields -> Range -> Record fields
setCharacterRange = setField

category :: HasField fields Category => Record fields -> Category
category = getField

setCategory :: HasField fields Category => Record fields -> Category -> Record fields
setCategory = setField

size :: HasField fields Size => Record fields -> Size
size = getField

setSize :: HasField fields Size => Record fields -> Size -> Record fields
setSize = setField

cost :: HasField fields Cost => Record fields -> Cost
cost = getField

setCost :: HasField fields Cost => Record fields -> Cost -> Record fields
setCost = setField


-- Instances

instance Arbitrary Size where
  arbitrary = Size <$> arbitrary

  shrink = fmap Size . shrink . unSize

instance Arbitrary Cost where
  arbitrary = Cost <$> arbitrary

  shrink = fmap Cost . shrink . unCost
