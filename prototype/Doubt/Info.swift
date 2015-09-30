public enum Info: Equatable {
	case Literal(String, [Category])
}


public func == (left: Info, right: Info) -> Bool {
	switch (left, right) {
	case let (.Literal(s1, c1), .Literal(s2, c2)):
		return s1 == s2 && c1 == c2
	}
}
