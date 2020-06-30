@a.b
class D:
  @f()
  @b(*b)
  @c(**c)
  @a(2, c=True, *a, **d)
  def f():
    g
