# CHECK-TREE: { ident <- \x -> x; y <- ident #unit; #record { ident: ident, y : y }}
def ident(x): return x
y = ident(())
