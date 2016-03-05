{-# LANGUAGE FlexibleContexts #-}
module Data.Option where

newtype Option a = Option { getOption :: Maybe a }

option :: b -> (a -> b) -> Option a -> b
option b f = maybe b f . getOption

maybeConcat :: (Foldable f, Monoid (Option a)) => f a -> Maybe a
maybeConcat = getOption . foldMap (Option. Just)
