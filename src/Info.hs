{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Info where

import Data.Record
import Prologue
import Category
import Range

newtype Size = Size { unSize :: Integer }
  deriving (Eq, Num, Show)
newtype Cost = Cost { unCost :: Integer }
  deriving (Eq, Num, Show)

type InfoFields = '[ Range, Category, Size, Cost ]

type Info' = Record InfoFields

characterRange' :: HasField fields Range => Record fields -> Range
characterRange' = getField

setCharacterRange' :: SetField fields Range => Record fields -> Range -> Record fields
setCharacterRange' = setField

category' :: HasField fields Category => Record fields -> Category
category' = getField

setCategory' :: SetField fields Category => Record fields -> Category -> Record fields
setCategory' = setField

size' :: HasField fields Size => Record fields -> Size
size' = getField

setSize' :: SetField fields Size => Record fields -> Size -> Record fields
setSize' = setField

cost' :: HasField fields Cost => Record fields -> Cost
cost' = getField

setCost' :: SetField fields Cost => Record fields -> Cost -> Record fields
setCost' = setField

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, category :: !Category, size :: !Integer, cost :: !Integer }
  deriving (Eq, Show)

setCharacterRange :: Info -> Range -> Info
setCharacterRange info range = info { characterRange = range }

setCategory :: Info -> Category -> Info
setCategory info category = info { category = category }

setSize :: Info -> Integer -> Info
setSize info size = info { size = size }

setCost :: Info -> Integer -> Info
setCost info cost = info { cost = cost }
