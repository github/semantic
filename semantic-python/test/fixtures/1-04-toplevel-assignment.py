# CHECK-JQ: .scope | has("hello") and has("goodbye")
# CHECK-TREE: { hello <- #true; goodbye <- #false; #record { hello: hello, goodbye: goodbye }}
hello = True
goodbye = False
