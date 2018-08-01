g = a b
g = a b c
g = a b c d

g = Just b
g = Right b
g = Example a c d
g = ()

a = [2..]
a = [2,3..]
a = [2..9]
a = [2,3..18]

b = [x | x <- xs]
b = [(x, y) | x <- xs, y <- ys]
b = [ x |  xs   <- [ [(10,20),(30,40)], [(50,40),(30,20)] ], (30,x) <- xs ]
b = [(i,j) | i <- [1,2,3,4],
             j <- [1..10] ]
b = [ [ (i,j) | i <- [2,4] ] | j <- [5..] ]

b = take 7 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]

b = (: a)
b = (:< a)

b = (b :)
b = (b :|)

b = h `i` j

b = Data.Just

parseJSON (JSON.Object s) = IncPK <$>
  s .: "id" <*>
  s .: "nullable_string" <*>
  s .: "non_nullable_string" <*>
  s .: "inserted_at"

g = do
  b <- d =<< e
  c <- f >>= h

g = \ x -> x

g = \ (Just a) -> a

g = \ x -> x : a : xs

g = \ g a b -> g <$> a <*> b

g = (-)
g = 1 - 1
g = (-1)
g = (-a)
g = -(1)

bar = catMaybes $ fmap (\q -> (q,) . Just <$> baz q) waz
