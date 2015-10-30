public enum JSONLeaf: Categorizable, CustomJSONConvertible, CustomStringConvertible, Equatable {
	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Null


	// MARK: Categorizable

	public var categories: Set<Swift.String> {
		switch self {
		case .Number:
			return [ "number" ]
		case .Boolean:
			return [ "boolean" ]
		case .String:
			return [ "string" ]
		case .Null:
			return [ "null" ]
		}
	}


	// MARK: CustomJSONConvertible

	public var JSON: Doubt.JSON {
		switch self {
		case let .Number(n):
			return .Number(n)
		case let .Boolean(b):
			return .Boolean(b)
		case let .String(s):
			return .String(s)
		case .Null:
			return .Null
		}
	}


	// MARK: CustomStringConvertible

	public var description: Swift.String {
		switch self {
		case let .Number(n):
			return Swift.String(n)
		case let .Boolean(b):
			return Swift.String(b)
		case let .String(s):
			return Swift.String(reflecting: s)
		case .Null:
			return "null"
		}
	}
}

public func == (left: JSONLeaf, right: JSONLeaf) -> Bool {
	switch (left, right) {
	case let (.Number(a), .Number(b)):
		return a == b
	case let (.Boolean(a), .Boolean(b)):
		return a == b
	case let (.String(a), .String(b)):
		return a == b
	case (.Null, .Null):
		return true
	default:
		return false
	}
}
