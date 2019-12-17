# CHECK-TREE: { const <- rec const = \x -> \y -> x; y <- const __semantic_prelude.True __semantic_prelude.True; z <- const __semantic_prelude.False __semantic_prelude.False; #record { const: const, y : y, z: z, }}
def const(x, y): return x
y = const(True, True)
z = const(False, False)
