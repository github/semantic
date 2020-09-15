module Data.Maybe.Exts
( module Data.Maybe
, maybeLast
, fromMaybeLast
, maybeM
) where

import Data.Maybe
import Data.Monoid

maybeLast :: Foldable t => b -> (a -> b) -> t a -> b
maybeLast b f = maybe b f . getLast . foldMap (Last . Just)

fromMaybeLast :: Foldable t => a -> t a -> a
fromMaybeLast b = fromMaybe b . getLast . foldMap (Last . Just)

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}
