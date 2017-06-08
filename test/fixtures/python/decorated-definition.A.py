@a.b
class C:
  @f()
  @d(1)
  @e(2, 3)
  @a(b=True)
  @a(*b)
  @a(**c)
  @a(1, b=True, *b, **c)
  def f():
    g
