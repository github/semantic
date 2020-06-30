class Foo
  def inspect
    "<foo>"
  end

  def foo
    "the foo method"
  end
end

class Bar < Foo
  def inspect
    "<bar>"
  end
end

class Bar
  def baz
  end
end

Bar.inspect()
