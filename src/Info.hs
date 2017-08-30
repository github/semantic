{-# LANGUAGE ConstraintKinds, DataKinds #-}
module Info
( Range(..)
, byteRange
, setByteRange
, Category(..)
, category
, setCategory
, Span(..)
, Pos(..)
, sourceSpan
, setSpan
) where

import Category
import Data.Range
import Data.Record
import Data.Span

byteRange :: HasField fields Range => Record fields -> Range
byteRange = getField

setByteRange :: HasField fields Range => Record fields -> Range -> Record fields
setByteRange = setField

category :: HasField fields Category => Record fields -> Category
category = getField

setCategory :: HasField fields Category => Record fields -> Category -> Record fields
setCategory = setField

sourceSpan :: HasField fields Span => Record fields -> Span
sourceSpan = getField

setSpan :: HasField fields Span => Record fields -> Span -> Record fields
setSpan = setField
