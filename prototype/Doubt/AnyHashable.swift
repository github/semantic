/// A Hashable instance for any type for which you can provide an equality function and a hash function.
public enum AnyHashable<A>: Hashable {
	public init(_ value: A, equals: (A, A) -> Bool, hash: A -> Int) {
		self = .External(.External(value, equals), hash)
	}

	case External(AnyEquatable<A>, A -> Int)

	public var value: A {
		switch self {
		case let .External(a, _):
			return a.value
		}
	}

	public var hashValue: Int {
		switch self {
		case let .External(a, hash):
			return hash(a.value)
		}
	}
}

public func == <A> (left: AnyHashable<A>, right: AnyHashable<A>) -> Bool {
	switch (left, right) {
	case let (.External(a, _), .External(b, _)):
		return a == b
	}
}
