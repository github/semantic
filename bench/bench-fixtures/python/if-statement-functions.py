def foo(): return "bipp"

def bar(): return foo()

def baz(): return baz()

def why(): return "elle"

if True:
    foo()
else:
    why()
