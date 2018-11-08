def Foo(x):
    if True:
        return x
    else:
        return 0

def Bar():
    def local():
        return 1
        
    return 0
