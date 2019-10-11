# CHECK-TREE: { Foo <- { identity <- \self -> \x -> x; type "Foo" object #record { identity: identity } }; #record { Foo: Foo } }

class Foo():
    def identity(self, x):
        return x
