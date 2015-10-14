//  Copyright © 2015 GitHub. All rights reserved.

public enum Cofree<A, B> {
	indirect case Unroll(B, Syntax<Cofree, A>)

	public var unwrap: Syntax<Cofree, A> {
		switch self {
		case let .Unroll(_, rest):
			return rest
		}
	}
}


// MARK: - Comonad

extension Cofree {
	public var extract: B {
		switch self {
		case let .Unroll(b, _):
			return b
		}
	}

	func extend<Other>(transform: Cofree -> Other) -> Cofree<A, Other> {
		return .Unroll(transform(self), unwrap.map { $0.extend(transform) })
	}
}
