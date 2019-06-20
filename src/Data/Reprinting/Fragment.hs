{-# LANGUAGE RankNTypes #-}

module Data.Reprinting.Fragment
  ( Fragment(..)
  , copy
  , insert
  , defer
  ) where

import Data.Text (Text)
import Streaming
import Streaming.Prelude (yield)

import Data.Reprinting.Scope
import Data.Reprinting.Token

-- | An intermediate representation of concrete syntax in the reprinting pipeline.
data Fragment
  = Verbatim Text
  -- ^ Verbatim copy of original 'Text' (un-refactored).
  | New Element [Scope] Text
  -- ^ New 'Text' to be inserted, along with original 'Element' and `Scope`
  -- allowing later steps to re-write.
  | Defer Element [Scope]
  -- ^ To be handled further down the pipeline.
  deriving (Eq, Show)

-- | Copy along some original, un-refactored 'Text'.
copy :: Monad m => Text -> Stream (Of Fragment) m ()
copy = yield . Verbatim

-- | Insert some new 'Text'.
insert :: Monad m => Element -> [Scope] -> Text -> Stream (Of Fragment) m ()
insert el c = yield . New el c

-- | Defer processing an element to a later stage.
defer :: Monad m => Element -> [Scope] -> Stream (Of Fragment) m ()
defer el = yield . Defer el
