# CHECK-TREE: { foo <- rec foo = \a -> if a then a else #unit; #record { foo: foo } }

def foo(a):
    if a: return a
    return ()
    ()
