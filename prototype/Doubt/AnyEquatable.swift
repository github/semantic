public enum AnyEquatable<A>: Equatable {
	case External(A, (A, A) -> Bool)
}

public func == <A> (left: AnyEquatable<A>, right: AnyEquatable<A>) -> Bool {
	switch (left, right) {
	case let (.External(a, eq), .External(b, _)):
		return eq(left, right)
	}
}
