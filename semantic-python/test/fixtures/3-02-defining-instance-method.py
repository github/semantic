# CHECK-TREE: { Foo <- rec Foo = { identity <- rec identity = \self -> \x -> x; __semantic_prelude.type "Foo" __semantic_prelude.object #record { identity: identity } }; #record { Foo: Foo } }

class Foo():
    def identity(self, x):
        return x
