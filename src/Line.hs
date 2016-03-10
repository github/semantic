module Line where

-- | A line of items or an empty line.
data Line a = Line [a] | Closed [a]
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Construct a single-element Line with a predicate determining whether the line is open.
pureBy :: (a -> Bool) -> a -> Line a
pureBy predicate a | predicate a = Line [ a ]
                   | otherwise = Closed [ a ]

unLine :: Line a -> [a]
unLine (Line as) = as
unLine (Closed as) = as

-- | Is the given line empty?
isEmpty :: Line a -> Bool
isEmpty = null . unLine

-- | Is the given line open?
isOpen :: Line a -> Bool
isOpen (Line _) = True
isOpen _ = False

-- | The increment the given line implies for line numbering.
lineIncrement :: Num n => Line a -> n
lineIncrement line | isEmpty line = 0
                   | otherwise = 1

-- | Transform the line by applying a function to a list of all the items in the
-- | line.
wrapLineContents :: ([a] -> b) -> Line a -> Line b
wrapLineContents transform line | isEmpty line = mempty
                                | otherwise = pureBy (const (isOpen line)) (transform (unLine line))

-- | Return the first item in the Foldable, or Nothing if it's empty.
maybeFirst :: Foldable f => f a -> Maybe a
maybeFirst = foldr (const . Just) Nothing

-- | Merge or prepend lines based on whether the left line is open or closed.
coalesceLines :: Line a -> Line a -> [Line a]
coalesceLines line nextLine | isOpen line = [line `mappend` nextLine]
                            | otherwise = [Closed (unLine line), nextLine]

instance Applicative Line where
  pure = Line . pure
  a <*> b = Line (unLine a <*> unLine b)

instance Monoid (Line a) where
  mempty = Line []
  mappend xs (Closed ys) = Closed (unLine xs `mappend` ys)
  mappend xs ys = Line (unLine xs `mappend` unLine ys)
