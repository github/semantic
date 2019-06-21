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

import Streaming
import Streaming.Prelude (yield)

import Data.Reprinting.Fragment

-- | The final representation of concrete syntax in the reprinting pipeline.
data Splice
  = Emit Text
  | Layout Whitespace
  deriving (Eq, Show)

-- | Emit some 'Text' as a 'Splice'.
emit :: Monad m => Text -> Stream (Of Splice) m ()
emit = yield . Emit

-- | Emit the provided 'Text' if the given predicate is true.
emitIf :: Monad m => Bool -> Text -> Stream (Of Splice) m ()
emitIf p = when p . emit

-- | Construct a layout 'Splice'.
layout :: Monad m => Whitespace -> Stream (Of Splice) m ()
layout = yield . Layout

-- | @indent w n@ emits @w@ 'Spaces' @n@ times.
indent :: Monad m => Int -> Int -> Stream (Of Splice) m ()
indent width times
  | times > 0 = replicateM_ times (layout (Indent width Spaces))
  | otherwise = pure ()

-- | Construct multiple layouts.
layouts :: Monad m => [Whitespace] -> Stream (Of Splice) m ()
layouts = traverse_ (yield . Layout)

-- | Single space.
space :: Monad m => Stream (Of Splice) m ()
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
