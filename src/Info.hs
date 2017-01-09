{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module Info (Range(..), characterRange, setCharacterRange, Category(..), category, setCategory, Cost(..), cost, setCost, SourceSpan(..), SourcePos(..), SourceSpans(..), SourceText(..), sourceText) where

import Data.Functor.Listable
import Data.Record
import Prologue
import Category
import Range
import SourceSpan
import Data.Aeson

newtype Cost = Cost { unCost :: Int }
  deriving (Eq, Num, Ord, Show, ToJSON)

newtype SourceText = SourceText { unText :: Text }
  deriving (Show, ToJSON)

characterRange :: HasField fields Range => Record fields -> Range
characterRange = getField

setCharacterRange :: HasField fields Range => Record fields -> Range -> Record fields
setCharacterRange = setField

category :: HasField fields Category => Record fields -> Category
category = getField

setCategory :: HasField fields Category => Record fields -> Category -> Record fields
setCategory = setField

cost :: HasField fields Cost => Record fields -> Cost
cost = getField

sourceText :: HasField fields SourceText => Record fields -> SourceText
sourceText = getField

setCost :: HasField fields Cost => Record fields -> Cost -> Record fields
setCost = setField


-- Instances

instance Listable Cost where
  tiers = cons1 Cost
