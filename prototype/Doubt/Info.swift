public enum Info: AlgebraicHashable {
	case Literal(String, Set<Category>)

	public var categories: Set<Category> {
		switch self {
		case let .Literal(_, c):
			return c
		}
	}

	public var hash: Hash {
		switch self {
		case let .Literal(source, categories):
			return Hash("Literal", Hash(source), Hash(categories))
		}
	}
}


public func == (left: Info, right: Info) -> Bool {
	switch (left, right) {
	case let (.Literal(s1, c1), .Literal(s2, c2)):
		return s1 == s2 && c1 == c2
	}
}
