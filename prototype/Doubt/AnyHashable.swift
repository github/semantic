public enum AnyHashable<A>: Hashable {
	case External(A, (A, A) -> Bool, A -> Int)

	public var value: A {
		switch self {
		case let .External(a, _, _):
			return a
		}
	}

	public var hashValue: Int {
		switch self {
		case let .External(a, _, hash):
			return hash(a)
		}
	}
}

public func == <A> (left: AnyHashable<A>, right: AnyHashable<A>) -> Bool {
	switch (left, right) {
	case let (.External(a, eq, _), .External(b, _, _)):
		return eq(a, b)
	}
}
