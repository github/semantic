# CHECK-TREE: { recursive <- rec recursive = \a -> recursive; #record { recursive: recursive }}
def recursive(a): return recursive
