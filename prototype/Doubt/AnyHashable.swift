public enum AnyHashable<A>: Hashable {
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
