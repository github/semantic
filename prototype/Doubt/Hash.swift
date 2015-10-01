public enum Hash: AlgebraicHashable {
	case Ordered([Hash])
	case Unordered([Hash])
	case Label(String)
	case Raw(Int)

	public init(_ label: String, _ hashes: Hash...) {
		self = .Ordered([ Hash(label) ] + hashes)
	}

	public init(_ string: String) {
		self = .Label(string)
	}

	public init(_ raw: Int) {
		self = .Raw(raw)
	}

	public init<A: AlgebraicHashable>(_ hashable: A) {
		self = hashable.hash
	}

	public init<A: Hashable>(_ hashable: A) {
		self = .Raw(hashable.hashValue)
	}


	public var hash: Hash {
		return self
	}

	public var hashValue: Int {
		switch self {
		case let .Ordered(s):
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
		case let .Unordered(s):
			return s.lazy.map { $0.hashValue }.reduce(0, combine: +)
		case let .Label(s):
			return s.hashValue
		case let .Raw(i):
			return i.hashValue
		}
	}
}

public func == (left: Hash, right: Hash) -> Bool {
	switch (left, right) {
	case let (.Ordered(a), .Ordered(b)):
		return a == b
	case let (.Unordered(a), .Unordered(b)):
		return a == b
	case let (.Label(a), .Label(b)):
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
