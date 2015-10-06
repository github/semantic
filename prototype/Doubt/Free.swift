/// The free monad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the monad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `Pure` case) to satisfy the monad laws.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Free` makes it recursive through the `Roll` case, and allows it to wrap values of type `B` through the `Pure` case.
///
/// In Doubt, this allows us to represent diffs as values of the `Free` monad obtained from `Syntax`, injecting `Patch` into the tree; or otherwise put, a diff is a tree of mutually-recursive `Free.Roll`/`Syntax` nodes with `Pure` nodes injecting the actual changes.
public enum Free<A, B> {
	/// The injection of a value of type `B` into the `Syntax` tree.
	case Pure(B)

	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case Roll(Syntax<Free, A>)

	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Syntax<Free, A> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(s):
			return ifRoll(s)
		}
	}

	public func iter(transform: Syntax<B, A> -> B) -> B {
		return analysis(
			ifPure: id,
			ifRoll: { transform($0.map { Free.iter($0)(transform) }) })
	}


	// MARK: Functor

	public func map<C>(@noescape transform: B -> C) -> Free<A, C> {
		return analysis(ifPure: { .Pure(transform($0)) }, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(@noescape transform: B -> Free<A, C>) -> Free<A, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}
}


// MARK: - Equality

extension Free {
	public static func equals(ifPure ifPure: (B, B) -> Bool, ifRoll: (A, A) -> Bool)(_ left: Free, _ right: Free) -> Bool {
		switch (left, right) {
		case let (.Pure(a), .Pure(b)):
			return ifPure(a, b)
		case let (.Roll(a), .Roll(b)):
			return Syntax.equals(ifLeaf: ifRoll, ifRecur: equals(ifPure: ifPure, ifRoll: ifRoll))(a, b)
		default:
			return false
		}
	}
}

public func == <A: Equatable, B: Equatable> (left: Free<A, B>, right: Free<A, B>) -> Bool {
	return Free.equals(ifPure: ==, ifRoll: ==)(left, right)
}


// MARK: - Hashing

extension Free {
	public func hash(ifPure ifPure: B -> Hash, ifRoll: A -> Hash) -> Hash {
		return analysis(ifPure: ifPure, ifRoll: { $0.hash(ifLeaf: ifRoll, ifRecur: { $0.hash(ifPure: ifPure, ifRoll: ifRoll) }) })
	}
}


extension Free where A: Hashable, B: Hashable {
	public var hash: Hash {
		return hash(ifPure: Hash.init, ifRoll: Hash.init)
	}
}
