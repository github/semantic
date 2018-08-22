module Data.Reprinting.Splice
  ( Splice(..)
  , emit
  , layout
  , layouts
  , space
  , indent

  , Datum(..)
  , copy
  , insert
  , raw

  , Whitespace(..)
  ) where

import Data.Reprinting.Token
import Data.Sequence (singleton, fromList)
import Prologue hiding (Element)

-- | The final representation of concrete syntax in the reprinting pipeline.
data Splice
  = Emit Text
  | Layout Whitespace
  deriving (Eq, Show)

-- | Emit some 'Text' as a 'Splice'.
emit :: Text -> Seq Splice
emit = singleton . Emit

-- | Construct a layout 'Splice'.
layout :: Whitespace -> Seq Splice
layout = singleton . Layout

-- | Construct multiple layouts.
layouts :: [Whitespace] -> Seq Splice
layouts = fromList . fmap Layout

-- | Single space.
space :: Seq Splice
space = layout Space

-- | Indent n times.
indent :: Integral b => b -> Seq Splice
indent times
  | times > 0 = stimes times (layout Indent)
  | otherwise = mempty

-- | An intermediate representation ....
-- The final representation of concrete syntax in the reprinting pipeline.
-- 'Inserts' have access to the original 'Element' and 'Context' for ease of
-- writing additional formatting steps in the reprinting pipeline.
data Datum
  = Original Text                 -- ^ Verbatim copy of original 'Text' (un-refactored).
  | Insert Element [Context] Text -- ^ New 'Text' to be inserted, along with original 'Element' and `Context`.
  | Raw Element [Context]         -- ^ To be handled further down the pipeline.
  deriving (Eq, Show)

-- | Copy along some original, un-refactored 'Text'.
copy :: Text -> Seq Datum
copy = singleton . Original

-- | Construct an 'Insert' datum.
insert :: Element -> [Context] -> Text -> Seq Datum
insert el c = singleton . Insert el c

-- | Construct an 'Raw' splice.
raw :: Element -> [Context] -> Seq Datum
raw el = singleton . Raw el

-- | Indentation, spacing, and other whitespace.
data Whitespace
  = HardWrap
  | SoftWrap
  | Space
  | Indent
  deriving (Eq, Show)
