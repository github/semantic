deriving instance Eq a => Eq (Foo a)
deriving instance Bar a (m a b c) => Bar a (Baz m a b c)
deriving instance Foo Baz foo => BazFail (FooEval foo bix waz)
deriving instance Far (Boo (Baz waz)) bix => BooHeap bix (FarEval bix wax)
