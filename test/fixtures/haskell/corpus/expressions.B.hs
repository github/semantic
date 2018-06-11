g = a b
g = a b c
g = a b c d

g = Just b
g = Right b
g = Example a c d
g = ()

b = [x | x <- xs]
b = [(x, y) | x <- xs, y <- ys]
b = [ x |  xs   <- [ [(10,20),(30,40)], [(50,40),(30,20)] ], (30,x) <- xs ]
