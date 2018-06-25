f = let y = x
        x = let g = 1
                in g
        in y

f = a
  where a = b
        b = 1

f = a
  where a = b
        b = 1

f = foo
  where a = b
          where c = d
                e = f
        x = w

g = do c
       a
       do b
          e
          do g
             g
          h
       i

a = do
  b
    where
    c = d

a = do
  b
  where
    c = d

a = do
  b
  where
  c = d
  
class Foo bar where
  fooVariables :: bar -> [Baz]
{-
-}
class Foo1 bar where
  liftFoo = foldMap

freeFoo bar = case freeFoo bar of
  [n] -> Right n
