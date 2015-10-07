public enum Category: AlgebraicHashable, Comparable, CustomDebugStringConvertible {
	case Tag(String)


	public var tag: String {
		switch self {
		case let .Tag(s):
			return s
		}
	}

	public var hash: Hash {
		return Hash("Tag", Hash(tag))
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case let .Tag(s):
			return ".Tag(\(s))"
		}
	}
}

public func == (left: Category, right: Category) -> Bool {
	return left.tag == right.tag
}

public func < (left: Category, right: Category) -> Bool {
	return left.tag < right.tag
}
