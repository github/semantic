def foo(): return "bipp"

def bar(): return foo()

def baz(): return bar()

def why(): return "elle"

if True:
    baz()
else:
    why()
