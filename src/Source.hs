module Source where

import Range
import qualified Data.Vector as Vector

newtype Source a = Source { getVector :: Vector.Vector a  }
  deriving (Eq, Show, Functor, Foldable)

fromList :: [a] -> Source a
fromList = Source . Vector.fromList

toList :: Source a -> [a]
toList = Vector.toList . getVector

slice :: Range -> Source a -> Source a
slice range = Source . Vector.slice (start range) (end range - start range) . getVector

toString :: Source Char -> String
toString = toList

at :: Source a -> Int -> a
at = (Vector.!) . getVector

null :: Source a -> Bool
null = Vector.null . getVector

cons :: a -> Source a -> Source a
cons a = Source . Vector.cons a . getVector

uncons :: Source a -> Maybe (a, Source a)
uncons (Source vector) = if Vector.null vector then Nothing else Just (Vector.head vector, Source $ Vector.tail vector)

break :: (a -> Bool) -> Source a -> (Source a, Source a)
break predicate (Source vector) = let (start, remainder) = Vector.break predicate vector in (Source start, Source remainder)

(++) :: Source a -> Source a -> Source a
(++) (Source a) = Source . (a Vector.++) . getVector

actualLines :: Source Char -> [Source Char]
actualLines source | Source.null source = [ source ]
actualLines source = case Source.break (== '\n') source of
  (l, lines') -> case uncons lines' of
    Nothing -> [ l ]
    Just (_, lines') -> (l Source.++ fromList "\n") : actualLines lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> Source Char -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous string = Range (end previous) $ end previous + length string
