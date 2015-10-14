//  Copyright © 2015 GitHub. All rights reserved.

/// The cofree comonad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the comonad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `B` paired with it) to satisfy the comonad laws.
///
/// This type is dual to `Free`. Where `Free` is inhabited by syntax trees where some terms are replaced with `B`s, `Cofree` is inhabited by syntax trees where all terms are annotated with `B`s. In Doubt, this allows us to e.g. annotate terms with source range information, categorization, etc.
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
