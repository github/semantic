{-# LANGUAGE RankNTypes #-}

module Data.Reprinting.Splice
  ( Fragment(..)
  , copy
  , insert
  , defer
  , Splice(..)
  , emit
  , emitIf
  , layout
  , indent
  , layouts
  , space
  , Whitespace(..)
  , Indentation(..)
  ) where

import Prologue hiding (Element)

import Data.Machine

import Data.Reprinting.Token

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
copy :: Text -> Plan k Fragment ()
copy = yield . Verbatim

-- | Insert some new 'Text'.
insert :: Element -> [Context] -> Text -> Plan k Fragment ()
insert el c = yield . New el c

-- | Defer processing an element to a later stage.
defer :: Element -> [Context] -> Plan k Fragment ()
defer el = yield . Defer el

-- | The final representation of concrete syntax in the reprinting pipeline.
data Splice
  = Emit Text
  | Layout Whitespace
  deriving (Eq, Show)

-- | Emit some 'Text' as a 'Splice'.
emit :: Text -> Plan k Splice ()
emit = yield . Emit

-- | Emit the provided 'Text' if the given predicate is true.
emitIf :: Bool -> Text -> Plan k Splice ()
emitIf p = when p . emit

-- | Construct a layout 'Splice'.
layout :: Whitespace -> Plan k Splice ()
layout = yield . Layout

-- | @indent w n@ emits @w@ 'Spaces' @n@ times.
indent :: Int -> Int -> Plan k Splice ()
indent width times
  | times > 0 = replicateM_ times (layout (Indent width Spaces))
  | otherwise = pure ()

-- | Construct multiple layouts.
layouts :: [Whitespace] -> Plan k Splice ()
layouts = traverse_ (yield . Layout)

-- | Single space.
space :: Plan k Splice ()
space = yield (Layout Space)

-- | Indentation, spacing, and other whitespace.
data Whitespace
  = HardWrap
  | SoftWrap
  | Space
  | Indent Int Indentation
  deriving (Eq, Show)

data Indentation = Tabs | Spaces
  deriving (Eq, Show)
