module Data.Duration
( Duration(..)
, fromSeconds
, fromMilliseconds
, toMicroseconds
) where

-- A duration suitable for timeouts.
newtype Duration = Milliseconds Int

fromMilliseconds :: Int -> Duration
fromMilliseconds n = Milliseconds (n * 1000)

fromSeconds :: Int -> Duration
fromSeconds n = Milliseconds (n * 1000)

toMicroseconds :: Duration -> Int
toMicroseconds (Milliseconds n) = n * 1000
