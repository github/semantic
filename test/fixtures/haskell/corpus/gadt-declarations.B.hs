data Bar a b c where
  Baz :: a -> b -> c -> Bar a b c

data Bar f a where
  Baz :: { jolo :: Maybe String, runJolo :: f a } -> Bar f a
