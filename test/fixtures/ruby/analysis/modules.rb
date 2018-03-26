module Bar
  def self.hi
    "<hi>"
  end
end

module Bar
  def self.hi
    "<hello>"
  end
end

Bar.hi()
