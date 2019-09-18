# CHECK-JQ: .scope | has("hello") and has("goodbye")
# CHECK-JQ: .tree.location.span | [.start.line, .start.column] == [0, 0]
# CHECK-JQ: .tree.location.span | [.end.line, .end.column] == [5, 0]
hello = ()
goodbye = ()
