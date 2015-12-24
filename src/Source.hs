module Source where

import Range
import qualified Data.Vector as Vector

newtype Source a = Source (Vector.Vector a)
  deriving (Eq, Show, Functor, Foldable)

makeSource :: [a] -> Source a
makeSource = Source . Vector.fromList

unSource :: Source a -> [a]
unSource (Source vector) = Vector.toList vector

subsource :: Range -> Source a -> Source a
subsource range (Source vector) = Source (Vector.slice (start range) (end range) vector)

toString :: Source Char -> String
toString = unSource

at :: Source a -> Int -> a
at = (!!) . unSource

null :: Source a -> Bool
null (Source vector) = Vector.null vector

uncons :: Source a -> Maybe (a, Source a)
uncons (Source vector) = if Vector.null vector then Nothing else Just (Vector.head vector, Source $ Vector.tail vector)
