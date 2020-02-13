def fac(n):
    if n == 0:
        return 1
    else:
        return n * fac(n-1)

fac(10)
