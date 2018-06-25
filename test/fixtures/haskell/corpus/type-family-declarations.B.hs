type family Baz bar where
  Baz = Wiz
  Bar = 'Custom
  Baz.Bar a = 'Custom
  Baz.Bar (B b) = 'Custom

type family F b :: *
type instance F [String] = Int
type instance F Char = Char

type family Baz (bar :: [(* -> *) -> Waz]) (faz :: * -> *) :: Waz where
