type Bar = Foo
type List' = []
type Foo a b = Bar a b
type Rec a = [Triangle a]
type X = ()
type Y = (,,)
type Z = (,)
type T = (->)
type Nat = Succ (Succ Zero) ': Succ Zero ': Zero ': '[]
type Wax bar baz wix = '[ W, A (B c) ]
type Bar Foo = "higher-kinded"
type Foo (Bar m a) = BarF m a
type Bar = HasCallStack => Foo []
type Bar a b = forall a b. BarF a b
type (MemberBar t r) = KnownNat (ElemIndexBar t r)
