# CHECK-TREE: { Foo <- rec Foo = __semantic_prelude.type "Foo" __semantic_prelude.object #record {}; #record { Foo: Foo }}
class Foo():
    pass
