f = a b
f = a b c
f = a b c d

f = Just b
f = Right b
f = Example a c d
f = ()

a = [x | x <- xs]
a = [(x, y) | x <- xs, y <- ys]
a = [ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], (3,x) <- xs ]
