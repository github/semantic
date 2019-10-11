# CHECK-TREE: { passthru <- \x -> x; decorated <- \x -> x; decorated = passthru(decorated); #record { passthru: passthru, decorated: decorated }}
def passthru(x):
    return x

@passthru
def decorated(x):
    return x
