module Data.Reprinting.Splice
  ( Splice(..)
  , Layout(..)
  , copy
  , unhandled
  , splice
  , layout
  , layouts
  ) where

import Data.Reprinting.Token
import Data.Sequence (singleton, fromList)
import Prologue hiding (Element)

-- | The final representation of concrete syntax in the reprinting pipeline.
-- 'Inserts' have access to the original 'Element' and 'Context' for ease of
-- writing additional formatting steps in the reprinting pipeline.
data Splice
  = Insert Element [Context] Text -- ^ New 'Text' to be inserted, along with original 'Element' and `Context`.
  | Original Text                 -- ^ Verbatim copy of original 'Text' (un-refactored).
  | Directive Layout              -- ^ Positional information (whitespace).
  | Unhandled Element [Context]   -- ^ To be handled further down the pipeline.
    deriving (Eq, Show)

-- | Copy along some original, un-refactored 'Text'.
copy :: Text -> Seq Splice
copy = singleton . Original

-- | Construct an 'Unhandled' splice.
unhandled :: Element -> [Context] -> Seq Splice
unhandled el = singleton . Unhandled el

-- | Construct a splice to insert.
splice :: Element -> [Context] -> Text -> Seq Splice
splice el c = singleton . Insert el c

-- | Construct a layout.
layout :: Layout -> Seq Splice
layout = singleton . Directive

-- | Construct multiple layouts.
layouts :: [Layout] -> Seq Splice
layouts = fromList . fmap Directive

-- | Indentation/spacing layouts.
data Layout
  = HardWrap
  | SoftWrap
  | Space
  | Indent
    deriving (Eq, Show)
