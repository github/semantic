/// A syntactic category to which nodes can belong.
public enum Category: AlgebraicHashable, Comparable, CustomDebugStringConvertible, CustomJSONConvertible {
	case Tag(String)


	public var tag: String {
		switch self {
		case let .Tag(s):
			return s
		}
	}


	// MARK: AlgebraicHashable

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


	// MARK: CustomJSONConvertible

	public var JSON: Doubt.JSON {
		switch self {
		case let .Tag(s):
			return [ "tag": .String(s) ]
		}
	}
}


// MARK: - Equatable

public func == (left: Category, right: Category) -> Bool {
	return left.tag == right.tag
}


// MARK: - Comparable

public func < (left: Category, right: Category) -> Bool {
	return left.tag < right.tag
}


// MARK: - Categorizable

/// A type whose values belong to a set of categories.
public protocol Categorizable {
	var categories: Set<Category> { get }
}
