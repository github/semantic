{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Info where

import Data.Record
import Prologue
import Category
import Range

newtype RangeA = RangeA { unRangeA :: Range }
newtype CategoryA = CategoryA { unCategoryA :: Category }
newtype SizeA = SizeA { unSizeA :: Integer }
newtype CostA = CostA { unCostA :: Integer }

type InfoFields = '[ RangeA, CategoryA, SizeA, CostA ]

type Info' = Record InfoFields

characterRange' :: HasField fields RangeA => Record fields -> Range
characterRange' = unRangeA . getField

setCharacterRange' :: SetField fields RangeA => Record fields -> Range -> Record fields
setCharacterRange' record = setField record . RangeA

category' :: HasField fields CategoryA => Record fields -> Category
category' = unCategoryA . getField

setCategory' :: SetField fields CategoryA => Record fields -> Category -> Record fields
setCategory' record = setField record . CategoryA

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
