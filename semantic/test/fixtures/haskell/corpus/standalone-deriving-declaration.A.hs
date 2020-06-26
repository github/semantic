deriving instance Eq b => Eq (Bar b)
deriving instance Baz a (m a b c) => Baz a (Bar m a b c)
deriving instance Bar Baz foo => BazFail (BarEval foo bix waz)
deriving instance Bar (Foo (Baz waz)) bix => BazHeap bix (BarEval bix wax)
