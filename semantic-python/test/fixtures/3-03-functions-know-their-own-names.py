# CHECK-TREE: { recursive <- rec recursive = \a -> recursive; #record { recursive: recursive }}
def recursive(a): return recursive

# this should scope correctly in the graph but it doesn't seem to yet
