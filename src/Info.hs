{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module Info
( Range(..)
, byteRange
, setCharacterRange
, Category(..)
, category
, setCategory
, Cost(..)
, SourceSpan(..)
, SourcePos(..)
, SourceSpans(..)
, sourceSpan
, setSourceSpan
, SourceText(..)
, sourceText
) where

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

byteRange :: HasField fields Range => Record fields -> Range
byteRange = getField

setCharacterRange :: HasField fields Range => Record fields -> Range -> Record fields
setCharacterRange = setField

category :: HasField fields Category => Record fields -> Category
category = getField

setCategory :: HasField fields Category => Record fields -> Category -> Record fields
setCategory = setField

sourceText :: HasField fields SourceText => Record fields -> SourceText
sourceText = getField

sourceSpan :: HasField fields SourceSpan => Record fields -> SourceSpan
sourceSpan = getField

setSourceSpan :: HasField fields SourceSpan => Record fields -> SourceSpan -> Record fields
setSourceSpan = setField

-- Instances

instance Listable Cost where
  tiers = cons1 Cost
