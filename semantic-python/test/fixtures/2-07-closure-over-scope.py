# CHECK-JQ: .scope.zilch[0].b[0].span  == { start: [8, 8], end: [ 8, 16 ] }
# CHECK-JQ: .scope.result[0].a[0].span == { start: [5, 8], end: [ 5, 16 ] }

def const(a, b):
    def result():
        return a

    def zilch(b):
        return b

    return result()
