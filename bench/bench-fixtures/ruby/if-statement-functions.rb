def foo()
  "bipp"
end

def bar()
  foo()
end

def baz()
  bar()
end

def why()
  return "elle"
end

if true
  baz()
else
  why()
end
