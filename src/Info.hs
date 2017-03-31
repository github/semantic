{-# LANGUAGE ConstraintKinds, DataKinds, GeneralizedNewtypeDeriving #-}
module Info
( HasDefaultFields
, Range(..)
, byteRange
, setCharacterRange
, Category(..)
, category
, setCategory
, SourceSpan(..)
, SourcePos(..)
, SourceSpans(..)
, sourceSpan
, setSourceSpan
, SourceText(..)
, sourceText
) where

import Data.Record
import Prologue
import Category
import Range
import SourceSpan
import Data.Aeson

-- | A type alias for HasField constraints commonly used throughout semantic-diff.
type HasDefaultFields fields = (HasField fields Category, HasField fields Range, HasField fields SourceSpan)

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
