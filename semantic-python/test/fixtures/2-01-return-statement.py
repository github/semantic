# CHECK-TREE: { foo <- rec foo = \a -> a; #record { foo: foo } }
def foo(a):
    return a
