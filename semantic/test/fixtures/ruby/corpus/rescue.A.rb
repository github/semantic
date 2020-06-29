begin
  foo
rescue Error
rescue StandardError, TimeoutError => x
  x
else
  z
ensure
  y
end

def foo
rescue Error
rescue StandardError, TimeoutError => x
else
ensure
end

bar rescue nil
