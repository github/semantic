class Object
  def new
    self
  end

  def inspect
    return "<object>"
  end
end

def puts(obj)
  __semantic_print(obj)
end
