f = a b
f = a b c
f = a b c d

f = Just b
f = Right b
f = Example a c d
f = ()

a = [1..]
a = [1,2..]
a = [1..2]
a = [1,2..10]

a = [x | x <- xs]
a = [(x, y) | x <- xs, y <- ys]
a = [ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], (3,x) <- xs ]
a = [(i,j) | i <- [1,2],
             j <- [1..4] ]
a = [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]

f = take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]

a = (: a)
a = (:< a)

a = (a :)
a = (a :|)

g = h `i` j

a = Data.Just

parseJSON (JSON.Object r) = IncPK <$>
  r .: "id" <*>
  r .: "nullable_string" <*>
  r .: "non_nullable_string" <*>
  r .: "inserted_at"

f = do
  a <- b =<< c
  b <- e >>= g

f = \ x -> x

f = \ (Just a) -> a

f = \ x -> x : a : xs

f = \ g a b -> g <$> a <*> b

f = (-)
f = 1 - 1
f = (-1)
f = (-a)
f = -(1)

foo = catMaybes $ fmap (\p -> (p,) . Just <$> bar p) waz
