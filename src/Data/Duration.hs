module Data.Duration
( Duration(..)
, fromSeconds
, fromMilliseconds
, fromMicroseconds
, fromNanoseconds
, toMicroseconds
, toSeconds
) where

-- A duration suitable for timeouts stored as an int of milliseconds.
newtype Duration = Milliseconds Int
  deriving (Eq, Ord)

instance Show Duration where
  showsPrec _ (Milliseconds n) = shows n <> showString "ms"

fromSeconds :: Int -> Duration
fromSeconds n = fromMilliseconds (n * 1000)

-- milli = 10E-3 seconds
fromMilliseconds :: Int -> Duration
fromMilliseconds n | n <= 0    = Milliseconds 0
                   | otherwise = Milliseconds n

-- micro = 10E-6 seconds
fromMicroseconds :: Int -> Duration
fromMicroseconds n = fromMilliseconds (n `div` 1000)

-- nano = 10E-9 seconds
fromNanoseconds :: Int -> Duration
fromNanoseconds n = fromMicroseconds (n `div` 1000)

toMicroseconds :: Duration -> Int
toMicroseconds (Milliseconds n) = n * 1000

toSeconds :: Duration -> Int
toSeconds (Milliseconds n) = n `div` 1000
