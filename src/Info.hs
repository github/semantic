{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Info where

import Data.Record
import Prologue
import Category
import Range

newtype SizeA = SizeA { unSizeA :: Integer }
newtype CostA = CostA { unCostA :: Integer }

type InfoFields = '[ Range, Category, SizeA, CostA ]

type Info' = Record InfoFields

characterRange' :: HasField fields Range => Record fields -> Range
characterRange' = getField

setCharacterRange' :: SetField fields Range => Record fields -> Range -> Record fields
setCharacterRange' = setField

category' :: HasField fields Category => Record fields -> Category
category' = getField

setCategory' :: SetField fields Category => Record fields -> Category -> Record fields
setCategory' = setField

size' :: HasField fields SizeA => Record fields -> Integer
size' = unSizeA . getField

setSize' :: SetField fields SizeA => Record fields -> Integer -> Record fields
setSize' record = setField record . SizeA

cost' :: HasField fields CostA => Record fields -> Integer
cost' = unCostA . getField

setCost' :: SetField fields CostA => Record fields -> Integer -> Record fields
setCost' record = setField record . CostA

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, category :: !Category, size :: !Integer, cost :: !Integer }
  deriving (Eq, Show)
