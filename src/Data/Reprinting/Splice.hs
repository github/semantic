module Data.Reprinting.Splice
  ( Fragment(..)
  , copy
  , insert
  , defer
  , Splice(..)
  , emit
  , layout
  , layouts
  , space
  , Whitespace(..)
  , Indentation(..)
  ) where

import Data.Reprinting.Token
import Data.Sequence (singleton, fromList)
import Prologue hiding (Element)

-- | An intermediate representation of concrete syntax in the reprinting pipeline.
data Fragment
  = Verbatim Text
  -- ^ Verbatim copy of original 'Text' (un-refactored).
  | New Element [Context] Text
  -- ^ New 'Text' to be inserted, along with original 'Element' and `Context`
  -- allowing later steps to re-write.
  | Defer Element [Context]
  -- ^ To be handled further down the pipeline.
  deriving (Eq, Show)

-- | Copy along some original, un-refactored 'Text'.
copy :: Text -> Seq Fragment
copy = singleton . Verbatim

-- | Insert some new 'Text'.
insert :: Element -> [Context] -> Text -> Seq Fragment
insert el c = singleton . New el c

-- | Defer processing an element to a later stage.
defer :: Element -> [Context] -> Seq Fragment
defer el = singleton . Defer el

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

-- | Indentation, spacing, and other whitespace.
data Whitespace
  = HardWrap
  | SoftWrap
  | Space
  | Indent Int Indentation
  deriving (Eq, Show)

data Indentation = Tabs | Spaces
  deriving (Eq, Show)
