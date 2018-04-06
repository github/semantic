def require_dependency(path)
  require_relative(path)
end

class Object
  def new
    self
  end

  def inspect
    return "<object>"
  end
end
