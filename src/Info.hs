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
, SourceSpan(..)
, SourcePos(..)
, SourceSpans(..)
, sourceSpan
, setSourceSpan
) where

import Category
import Data.Record
import Range
import SourceSpan

-- | The default set of fields produced by our parsers.
type DefaultFields = '[ Range, Category, SourceSpan ]

-- | A type alias for HasField constraints commonly used throughout semantic-diff.
type HasDefaultFields fields = (HasField fields Category, HasField fields Range, HasField fields SourceSpan)

byteRange :: HasField fields Range => Record fields -> Range
byteRange = getField

setByteRange :: HasField fields Range => Record fields -> Range -> Record fields
setByteRange = setField

category :: HasField fields Category => Record fields -> Category
category = getField

setCategory :: HasField fields Category => Record fields -> Category -> Record fields
setCategory = setField

sourceSpan :: HasField fields SourceSpan => Record fields -> SourceSpan
sourceSpan = getField

setSourceSpan :: HasField fields SourceSpan => Record fields -> SourceSpan -> Record fields
setSourceSpan = setField
