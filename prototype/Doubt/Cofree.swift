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


// MARK: - Functor

extension Cofree {
	public func map<Other>(transform: B -> Other) -> Cofree<A, Other> {
		return .Unroll(transform(extract), unwrap.map { $0.map(transform) })
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

	public func extend<Other>(transform: Cofree -> Other) -> Cofree<A, Other> {
		return .Unroll(transform(self), unwrap.map { $0.extend(transform) })
	}

	public var duplicate: Cofree<A, Cofree<A, B>> {
		return .Unroll(self, unwrap.map { $0.duplicate })
	}
}
