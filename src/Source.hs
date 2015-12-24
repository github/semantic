module Source where

import Range
import qualified Data.Vector as Vector

newtype Source a = Source { getVector :: Vector.Vector a  }
  deriving (Eq, Show, Functor, Foldable)

makeSource :: [a] -> Source a
makeSource = Source . Vector.fromList

unSource :: Source a -> [a]
unSource (Source vector) = Vector.toList vector

subsource :: Range -> Source a -> Source a
subsource range (Source vector) = Source $ Vector.slice (start range) (end range - start range) vector

toString :: Source Char -> String
toString = unSource

at :: Source a -> Int -> a
at = (!!) . unSource

null :: Source a -> Bool
null (Source vector) = Vector.null vector

cons :: a -> Source a -> Source a
cons a = Source . Vector.cons a . getVector

uncons :: Source a -> Maybe (a, Source a)
uncons (Source vector) = if Vector.null vector then Nothing else Just (Vector.head vector, Source $ Vector.tail vector)

break :: (a -> Bool) -> Source a -> (Source a, Source a)
break predicate (Source vector) | (start, remainder) <- Vector.break predicate vector = (Source start, Source remainder)
