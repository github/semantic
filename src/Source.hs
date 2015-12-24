module Source where

import Range
import qualified Data.Vector as Vector

newtype Source a = Source { getVector :: Vector.Vector a  }
  deriving (Eq, Show, Functor, Foldable)

makeSource :: [a] -> Source a
makeSource = Source . Vector.fromList

toList :: Source a -> [a]
toList = Vector.toList . getVector

slice :: Range -> Source a -> Source a
slice range = Source . Vector.slice (start range) (end range - start range) . getVector

toString :: Source Char -> String
toString = toList

at :: Source a -> Int -> a
at = (!!) . toList

null :: Source a -> Bool
null (Source vector) = Vector.null vector

cons :: a -> Source a -> Source a
cons a = Source . Vector.cons a . getVector

uncons :: Source a -> Maybe (a, Source a)
uncons (Source vector) = if Vector.null vector then Nothing else Just (Vector.head vector, Source $ Vector.tail vector)

break :: (a -> Bool) -> Source a -> (Source a, Source a)
break predicate (Source vector) = let (start, remainder) = Vector.break predicate vector in (Source start, Source remainder)
