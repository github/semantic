{-# LANGUAGE FlexibleContexts #-}
module Data.Option where

import Prologue

newtype Option a = Option { getOption :: Maybe a }

option :: b -> (a -> b) -> Option a -> b
option b f = maybe b f . getOption

-- | Return Just the concatenation of any elements in a Foldable, or Nothing if it is empty.
maybeConcat :: (Foldable f, Monoid (Option a)) => f a -> Maybe a
maybeConcat = getOption . foldMap (Option. Just)
