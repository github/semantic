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

category' :: HasField fields CategoryA => Record fields -> Category
category' = unCategoryA . getField

size' :: HasField fields SizeA => Record fields -> Integer
size' = unSizeA . getField

cost' :: HasField fields CostA => Record fields -> Integer
cost' = unCostA . getField

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, category :: !Category, size :: !Integer, cost :: !Integer }
  deriving (Eq, Show)
