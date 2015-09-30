public enum Category: AlgebraicHashable {
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
}

public func == (left: Category, right: Category) -> Bool {
	switch (left, right) {
	case let (.Tag(a), .Tag(b)):
		return a == b
	}
}
