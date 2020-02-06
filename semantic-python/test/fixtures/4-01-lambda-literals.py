# CHECK-TREE: { const <- \x -> \y -> x; y <- const #true #true; z <- const #false #false; #record { const: const, y : y, z: z, }}
const = lambda x, y: x
y = const(True, True)
z = const(False, False)
