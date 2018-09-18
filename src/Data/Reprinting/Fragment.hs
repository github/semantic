{-# LANGUAGE RankNTypes #-}

module Data.Reprinting.Fragment
  ( Fragment(..)
  , copy
  , insert
  , defer
  ) where

import Data.Machine
import Data.Text (Text)

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
