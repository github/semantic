class Foo
  def inspect
    "<foo>"
  end
end

class Bar < Foo
  def inspect
    "<bar>"
  end
end

Bar.inspect()
