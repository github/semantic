/// An Equatable instance for any type for which you can provide an equality function.
///
/// This can enable equating [T] where T does not conform to Equatable, by first mapping to [AnyEquatable<T>] and then comparing with ==.
public enum AnyEquatable<A>: Equatable {
	case External(A, (A, A) -> Bool)

	public var value: A {
		switch self {
		case let .External(a, _):
			return a
		}
	}
}

public func == <A> (left: AnyEquatable<A>, right: AnyEquatable<A>) -> Bool {
	switch (left, right) {
	case let (.External(a, eq), .External(b, _)):
		return eq(a, b)
	}
}
