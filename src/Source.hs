module Source where

import Range

newtype Source a = Source (Int, [a])
  deriving (Eq, Show, Functor, Foldable)

makeSource :: [a] -> Source a
makeSource list = Source (0, list)

unSource :: Source a -> [a]
unSource (Source (_, list)) = list

subsource :: Range -> Source a -> Source a
subsource range (Source (i, list)) = Source (start range, sublist (offsetRange (negate i) range) list)

toString :: Source Char -> String
toString = unSource

at :: Source a -> Int -> a
at = (!!) . unSource
