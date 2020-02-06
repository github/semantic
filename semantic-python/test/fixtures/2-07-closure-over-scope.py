
def const(a, b):
    def result():
        return a

    def zilch(b):
        return b

    return result()
