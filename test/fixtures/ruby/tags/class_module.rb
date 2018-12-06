# Public: Foo
module Foo

  # Public: Bar
  class Bar

    # Public: baz
    def baz(a)
      a * 10
    end
  end
end

class A::B::C
  def foo
    puts "hi"
  end
  def self.foo
  end
end
