module Bar
  def self.hi
    "<hi>"
  end
end

module Bar
  def self.hi
    "<hello>"
  end
  module Baz
    def self.baz
      "<baz>"
    end
  end
end

Bar.hi()
