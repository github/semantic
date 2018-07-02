f = let x = y
        y = let h = 1
                in h
        in x

f = b
  where b = a
        a = 1

f = c
  where c = b
        b = 1

f = bar
  where c = a
          where a = e
                e = f
        w = x

g = do a
       b
       do c
          d
          do e
             f
          g
       h

b = do
  a
    where
    d = c

b = do
  c
  where
    c = e

b = do
  a
  where
  a = f
