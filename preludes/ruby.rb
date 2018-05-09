class Object
  def new
    self
  end

  def inspect
    return "<object>"
  end
end

def puts(obj)
  __builtin_print(obj)
end
