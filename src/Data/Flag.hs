-- | -- This technique is due to Oleg Grenrus: <http://oleg.fi/gists/posts/2019-03-21-flag.html>
-- The implementation is clean-room due to unclear licensing of the original post.
module Data.Flag
  ( Flag
  , flag
  , toBool
  , switch
  , choose
  ) where

import Prologue

-- | To declare a new flag, declare a singly-inhabited type:
-- @data MyFlag = MyFlag@
-- then use the @flag MyFlag@ to create one from a 'Bool'.
-- This is more verbose than using 'Bool' for everything but prevents classes of errors when
-- working with multiple flag values in flight, as the 'toBool' deconstructor provides a witness
-- that you really want the given semantic flag value from the flag datum.
newtype Flag t = Flag Bool
  deriving (Eq, Show)

-- | The constructor for a 'Flag'. You specify @t@ with a visible type application.
flag :: t -> Bool -> Flag t
flag _ = Flag
{-# INLINE flag #-}

-- | The destructor for a 'Flag'. You pass in the inhabitant of @t@ to
-- avoid boolean blindness.
toBool :: t -> Flag t -> Bool
toBool _ = coerce
{-# INLINE toBool #-}

switch :: a -> b -> Flag a -> Flag b
switch _ _ = coerce

-- | Case analysis, like 'bool'.
choose :: t -- ^ Witness
       -> a -- ^ False case
       -> a -- ^ True case
       -> Flag t
       -> a
choose _ f t flag = if coerce flag then t else f
{-# INLINE choose #-}
