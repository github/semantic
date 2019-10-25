# CHECK-TREE: { const <- rec const = \x -> \y -> x; y <- const #true #true; z <- const #false #false; #record { const: const, y : y, z: z, }}
def const(x, y): return x
y = const(True, True)
z = const(False, False)
