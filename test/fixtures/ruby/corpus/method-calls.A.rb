x.foo()

foo(a, b, :c => 2, d: 3)
foo(bar(a),)
foo([] => 1, *bar, &blk, -> (a) { 1 })
