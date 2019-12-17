# CHECK-TREE: { const <- \x -> \y -> x; y <- const __semantic_prelude.True __semantic_prelude.True; #record { const: const, y : y }}
const = lambda x, y: x
y = const(True, True)
