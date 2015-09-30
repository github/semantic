public enum Info: Equatable {
	case Literal(String, Set<Category>)

	public var categories: Set<Category> {
		switch self {
		case let .Literal(_, c):
			return c
		}
	}
}


public func == (left: Info, right: Info) -> Bool {
	switch (left, right) {
	case let (.Literal(s1, c1), .Literal(s2, c2)):
		return s1 == s2 && c1 == c2
	}
}
