class A
  def self.hi
    "<hi>"
  end
end

class A
  def self.hi
    "<hello>"
  end
  class B
    def self.hi
      "<B>"
    end
  end
end

A.hi()
