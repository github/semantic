type family Foo bar where
  Bar = Wiz
  Baz = 'Custom
  Bar.Baz a = 'Custom
  Bar.Baz (A a) = 'Custom

type family F a :: *
type instance F [Int] = Int
type instance F String = Char

type family Bar (baz :: [(* -> *) -> Wiz]) (foo :: * -> *) :: Wiz where
