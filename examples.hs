Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2)
   ,(Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,
        Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_b"} :# 3,
   Declaration {symbol = "_b", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 6)
   ]

Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,
        Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2)
    ,(Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3),
    (Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,
        Scope {symbol = "_b"} :# 3),
    (Scope {symbol = "_b"} :# 3,
        Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4),
    (Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,
        Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)]

Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2),(Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_a"} :# 1,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ]

Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2)
   ,(Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Scope {symbol = "_a"} :# 1)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ]
Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2)
   ,(Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 6)
   ,(Scope {symbol = "__main__"} :# 4,Scope {symbol = "_a"} :# 1)
   ,(Scope {symbol = "__main__"} :# 4,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "__main__"} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Scope {symbol = "__main__"} :# 4,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 6)
   ,(Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5,Scope {symbol = "_a"} :# 1)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 6,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ]

Right {files {fromList [
    (Scope {symbol = "_a"} :# 1,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2),
    (Scope {symbol = "_a"} :# 1,Scope {symbol = "_b"} :# 3)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 2,Scope {symbol = "_b"} :# 3)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Scope {symbol = "_a"} :# 1)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ]

Right {files {fromList [
    (Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 6)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1)
   ]

Right {files {fromList [
    (Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 6,Scope {symbol = "_b"} :# 3)
   ]

Right {files {fromList [
    (Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 5)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 6,Scope {symbol = "_b"} :# 3)
   ]

Right {files {fromList [
    (Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 5)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 1)
   ,(Scope {symbol = "__main__"} :# 5,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 6)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 14}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 6,Scope {symbol = "_b"} :# 3)
   ]

Right {files {fromList [
    (Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 9)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,PushSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(PopSymbol {symbol = "."} :# 5,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(PopSymbol {symbol = "."} :# 5,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(PopSymbol {symbol = "."} :# 5,Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6)
   ,(PopSymbol {symbol = "."} :# 5,PushSymbol {symbol = "."} :# 7)
   ,(PopSymbol {symbol = "."} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,PushSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(PushSymbol {symbol = "."} :# 7,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(PushSymbol {symbol = "."} :# 7,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(Scope {symbol = "__main__"} :# 9,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10,Scope {symbol = "_b"} :# 3)
   ]

Right {files {fromList [
    (Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,PushSymbol {symbol = "."} :# 7)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 9)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,PopSymbol {symbol = "."} :# 5)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,PushSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(PopSymbol {symbol = "."} :# 5,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(PopSymbol {symbol = "."} :# 5,Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6)
   ,(PopSymbol {symbol = "."} :# 5,PushSymbol {symbol = "."} :# 7)
   ,(PopSymbol {symbol = "."} :# 5,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,PushSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(PushSymbol {symbol = "."} :# 7,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 8)
   ,(Scope {symbol = "__main__"} :# 9,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10,Scope {symbol = "_b"} :# 3)
   ]

Right {files {fromList
   [
    (Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,PushSymbol {symbol = "."} :# 8)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 9)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,PopSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(PopSymbol {symbol = "."} :# 7,Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6)
   ,(PushSymbol {symbol = "."} :# 8,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Scope {symbol = "__main__"} :# 9,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10,Scope {symbol = "_b"} :# 3)
   ]

2 -> 2 ["ints",".","cheese","__main__"] ["ints"] -- Root to Root with a starting scope stack [ints, ., cheese, __main__] and ending at an ints ending symbol stack
1 -> 2 "ints" -- ints to Root node
2 -> 2 "" -- Root node to Root node
2 -> 10 "__main__" -- Root to __main__ Module declaration
2 -> 4 ["cheese", "__main__"] -- cheese reference to cheese declaration with cheese.__main__ scope stack
2 -> 6 ["ints",".","cheese","__main__"] -- Root to ints declaration with ints.cheese.__main__ starting symbol stack
[SGPath {
    pathStartingSymbolStack = ["ints",".","cheese","__main__"]
  , pathStartingScopeStackSize = 0
  , pathFrom = 2
  , pathEdges = ""
  , pathTo = 2
  , pathEndingScopeStack = []
  , pathEndingSymbolStack = ["ints"]}
,SGPath {
    pathStartingSymbolStack = ["ints",".","cheese","__main__"]
    , pathStartingScopeStackSize = 0
    , pathFrom = 2
    , pathEdges = ""
    , pathTo = 6
    , pathEndingScopeStack = []
    , pathEndingSymbolStack = []}
,SGPath {
    pathStartingSymbolStack = ["cheese","__main__"]
    , pathStartingScopeStackSize = 0
    , pathFrom = 2
    , pathEdges = ""
    , pathTo = 4
    , pathEndingScopeStack = []
    , pathEndingSymbolStack = []}
,SGPath {pathStartingSymbolStack = ["__main__"]
    , pathStartingScopeStackSize = 0
    , pathFrom = 2
    , pathEdges = ""
    , pathTo = 10
    , pathEndingScopeStack = []
    , pathEndingSymbolStack = []}
,SGPath {pathStartingSymbolStack = []
    , pathStartingScopeStackSize = 0
    , pathFrom = 2
    , pathEdges = ""
    , pathTo = 2
    , pathEndingScopeStack = []
    , pathEndingSymbolStack = []}
,SGPath {
    pathStartingSymbolStack = []
  , pathStartingScopeStackSize = 0
  , pathFrom = 1
  , pathEdges = ""
  , pathTo = 2
  , pathEndingScopeStack = []
  , pathEndingSymbolStack = ["ints"]}]

Right {files {fromList
   [
    (Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,Scope {symbol = "_a"} :# 2)
   ,(Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1,PushSymbol {symbol = "."} :# 8)
   ,(Scope {symbol = "_a"} :# 2,Scope {symbol = "__main__"} :# 9)
   ,(Scope {symbol = "_b"} :# 3,Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Declaration {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 4,PopSymbol {symbol = "."} :# 7)
   ,(Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6,Reference {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 1)
   ,(PopSymbol {symbol = "."} :# 7,Declaration {symbol = "ints", kind = Identifier, location = Loc {byteRange = Range {start = 14, end = 18}, span = Span {start = Pos {line = 0, column = 14}, end = Pos {line = 0, column = 18}}}} :# 6)
   ,(PushSymbol {symbol = "."} :# 8,Reference {symbol = "cheese", kind = Identifier, location = Loc {byteRange = Range {start = 7, end = 13}, span = Span {start = Pos {line = 0, column = 7}, end = Pos {line = 0, column = 13}}}} :# 5)
   ,(Scope {symbol = "__main__"} :# 9,Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10)
   ,(Declaration {symbol = "__main__", kind = Identifier, location = Loc {byteRange = Range {start = 0, end = 19}, span = Span {start = Pos {line = 0, column = 0}, end = Pos {line = 1, column = 0}}}} :# 10,Scope {symbol = "_b"} :# 3)
   ]


   2 -> 2  ""                                                       -- Project to Project
   2 -> 10 "6-02-qualified-imports"                                 -- Project to File Module Declaration
   2 -> 2  ["6-02-qualified-imports", "cheese",".","ints"] ["ints"] -- Project to Project Path with module.cheese.ints starting stack and ints ending stack
   2 -> 6  ["6-02-qualified-imports", "cheese",".","ints"]          -- Project to ints Declaration Path with module.cheese.ints starting symbol stack
   2 -> 4  ["6-02-qualified-imports", "cheese"]                     -- cheese Reference to cheese Declaration Path with module.cheese starting symbol stack
   1 -> 2  "ints"                                                   -- ints Reference to Project with ints ending stack
