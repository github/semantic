class Object
  def self.new
    self
  end

  def self.inspect
    return "Object"
  end

  def inspect
    return "\#<Object:0x...>"
  end
end
