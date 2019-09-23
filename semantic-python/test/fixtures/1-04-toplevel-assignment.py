# CHECK-JQ: .scope | has("hello") and has("goodbye")
# CHECK-TREE: { hello <- #unit; goodbye <- #unit; #record { hello: hello, goodbye: goodbye }}
hello = ()
goodbye = ()
