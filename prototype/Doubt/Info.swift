/// Source info & categorization for nodes in a syntax tree.
public enum Info: AlgebraicHashable, Categorizable, CustomDebugStringConvertible, CustomJSONConvertible {
	case Literal(String, Set<Category>)

	public var categories: Set<Category> {
		switch self {
		case let .Literal(_, c):
			return c
		}
	}


	// MARK: AlgebraicHashable

	public var hash: Hash {
		switch self {
		case let .Literal(source, categories):
			return Hash("Literal", Hash(source), Hash(categories))
		}
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case let .Literal(s, c) where c.isEmpty:
			return s
		case let .Literal(s, c):
			return s + " (" + c.sort().map { String(reflecting: $0) }.joinWithSeparator(", ") + ")"
		}
	}


	// MARK: CustomJSONConvertible

	public var JSON: Doubt.JSON {
		switch self {
		case let .Literal(source, categories):
			return [
				"source": .String(source),
				"categories": .Array(categories.map { $0.JSON }),
			]
		}
	}
}


// MARK: - Equality

public func == (left: Info, right: Info) -> Bool {
	switch (left, right) {
	case let (.Literal(s1, c1), .Literal(s2, c2)):
		return s1 == s2 && c1 == c2
	}
}
