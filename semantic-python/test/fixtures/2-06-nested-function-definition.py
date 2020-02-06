# CHECK-TREE: { const <- rec const = \a -> \b -> { identity <- rec identity = \x -> x; identity a }; #record{ const: const }}

def const(a, b):
    def identity(x):
        return x

    return identity(a)
