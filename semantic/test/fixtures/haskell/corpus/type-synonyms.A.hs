type Foo = Bar
type List = []
type Foo a = Bar a
type Rec a = [Circ a]
type V = ()
type X = (,)
type Y = (,,)
type Z = (->)
type Nat = Zero ': Succ Zero ': Succ (Succ Zero) ': '[]
type Foo bar baz wix = '[ W, A (B c) ]
type Foo Bar = "higher-kinded"
type Bar (Foo m a) = BarF m a
type Foo = HasCallStack => Bar []
type Foo a b = forall a b. FooF a b
type (Member t r) = KnownNat (ElemIndex t r)
