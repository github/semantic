data Foo a b c where
  Baz :: a -> b -> c -> Foo a b c

data Foo f a where
  Bar :: { jolo :: Maybe String, runJolo :: f a } -> Foo f a

data Foo a :: [*] -> * where
