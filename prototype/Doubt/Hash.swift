public enum Hash: Hashable {
	case Sequence([Hash])
	case String(Swift.String)
	case Raw(Swift.Int)

	public static func Case(label: Swift.String, _ hashes: Hash...) -> Hash {
		return .Sequence([ .String(label) ] + hashes)
	}

	public static func Case(index: Swift.Int, _ hashes: Hash...) -> Hash {
		return .Sequence([ .Raw(index) ] + hashes)
	}

	public init<A: AlgebraicHashable>(_ hashable: A) {
		self = hashable.hash
	}

	public init<A: Hashable>(_ hashable: A) {
		self = .Raw(hashable.hashValue)
	}


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
		case let .Raw(i):
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
	case let (.Raw(a), .Raw(b)):
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

extension RawRepresentable where RawValue: Hashable {
	public var hash: Hash {
		return Hash(rawValue)
	}
}

extension RawRepresentable where RawValue: AlgebraicHashable {
	public var hash: Hash {
		return Hash(rawValue)
	}
}
