# CHECK-JQ: .tree.contents[0][1].contents[1] | .tag == "Lam" and .contents.value.tag == "If"
# CHECK-JQ: .tree.contents[0][1].contents[1].contents.value | .contents == [[], [], { "tag": "Unit" }]

def foo(a):
    if a: return a
    return ()
    ()
