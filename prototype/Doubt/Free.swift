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

	/// A recursive instantiation of `Syntax`, unrolling another loop of the recursive type.
	indirect case Roll(Syntax<Free, A>)

	public func map<C>(@noescape transform: B -> C) -> Free<A, C> {
		switch self {
		case let .Pure(b):
			return .Pure(transform(b))
		case let .Roll(s):
			return .Roll(s.map { $0.map(transform) })
		}
	}

	public func flatMap<C>(@noescape transform: B -> Free<A, C>) -> Free<A, C> {
		switch self {
		case let .Pure(b):
			return transform(b)
		case let .Roll(s):
			return .Roll(s.map { $0.flatMap(transform) })
		}
	}
}
