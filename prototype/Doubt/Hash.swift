public enum Hash: Hashable {
	case Sequence([Hash])
	case String(Swift.String)
	case Int(Swift.Int)


	public var hashValue: Swift.Int {
		switch self {
		case let .Sequence(s):
			// Bob Jenkins’ one-at-a-time hash: https://en.wikipedia.org/wiki/Jenkins_hash_function
			var hash = 0
			for each in s {
				hash += each.hashValue
				hash += hash << 10
				hash ^= hash >> 6
			}
			hash += hash << 3
			hash ^= hash >> 11
			hash += hash << 15
			return hash
		case let .String(s):
			return s.hashValue
		case let .Int(i):
			return i.hashValue
		}
	}
}

public func == (left: Hash, right: Hash) -> Bool {
	switch (left, right) {
	case let (.Sequence(a), .Sequence(b)):
		return a == b
	case let (.String(a), .String(b)):
		return a == b
	case let (.Int(a), .Int(b)):
		return a == b
	default:
		return false
	}
}

public protocol AlgebraicHashable: Hashable {
	var hash: Hash { get }
}

extension AlgebraicHashable {
	public var hashValue: Int {
		return hash.hashValue
	}
}

