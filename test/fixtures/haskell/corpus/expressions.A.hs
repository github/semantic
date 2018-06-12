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
