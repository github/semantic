{-# LANGUAGE ConstraintKinds, DataKinds #-}
module Info
( DefaultFields
, HasDefaultFields
, Range(..)
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
import Data.Record
import Range
import SourceSpan

-- | The default set of fields produced by our parsers.
type DefaultFields = '[ Range, Category, Span ]

-- | A type alias for HasField constraints commonly used throughout semantic-diff.
type HasDefaultFields fields = (HasField fields Category, HasField fields Range, HasField fields Span)

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
