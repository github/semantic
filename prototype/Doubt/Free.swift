/// The free monad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the monad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `Pure` case) to satisfy the monad laws.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Free` makes it recursive through the `Roll` case, and allows it to wrap values of type `B` through the `Pure` case.
///
/// In Doubt, this allows us to represent diffs as values of the `Free` monad obtained from `Syntax`, injecting `Patch` into the tree; or otherwise put, a diff is a tree of mutually-recursive `Free.Roll`/`Syntax` nodes with `Pure` nodes injecting the actual changes.
public enum Free<A, B>: CustomDebugStringConvertible {
	/// The injection of a value of type `B` into the `Syntax` tree.
	case Pure(B)

	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case Roll(Syntax<Free, A>)


	/// Recursively copies a `Fix<A>` into a `Free<A, B>`, essentially mapping `Fix.In` onto `Free.Roll`.
	public init(_ fix: Fix<A>) {
		self = .Roll(fix.out.map(Free.init))
	}


	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Syntax<Free, A> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(s):
			return ifRoll(s)
		}
	}

	/// Reduce the receiver by iteration.
	///
	/// `Pure` values are simply unpacked. `Roll` values are mapped recursively, and then have `transform` applied to them.
	///
	/// This forms a _catamorphism_ (from the Greek “cata”, “downwards”; compare “catastrophe”), a generalization of folds over regular trees (and datatypes isomorphic to them). It operates at the leaves first, and then branches near the periphery, recursively collapsing values by whatever is computed by `transform`. Catamorphisms are themselves an example of _recursion schemes_, which characterize specific well-behaved patterns of recursion. This gives `iterate` some useful properties for computations performed over trees.
	///
	/// Due to the character of recursion captured by catamorphisms, `iterate` ensures that computation will not only halt, but will further be linear in the size of the receiver. (Nesting a call to `iterate` will therefore result in O(n²) complexity.) This guarantee is achieved by careful composition of calls to `map` with recursive calls to `iterate`, only calling `transform` once the recursive call has completed. `transform` is itself non-recursive, receiving a `Syntax` whose recurrences have already been flattened to `B`.
	///
	/// The linearity of `iterate` in the size of the receiver makes it trivial to compute said size, by counting leaves as 1 and summing branches’ children:
	///
	///		func size<A, B>(free: Free<A, B>) -> Int {
	///			return free.iterate { flattenedSyntax in
	///				switch flattenedSyntax {
	///				case .Leaf:
	///					return 1
	///				case let .Indexed(children):
	///					return children.reduce(0, combine: +)
	///				case let .Keyed(children):
	///					return children.lazy.map { $1 }.reduce(0, combine: +)
	///				}
	///			}
	///		}
	///
	/// While not every function on a given `Free` can be computed using `iterate`, these guarantees of termination and complexity, as well as the brevity and focus on the operation being performed n times, make it a desirable scaffolding for any function which can.
	///
	/// For a lucid, in-depth tutorial on recursion schemes, I recommend [Patrick Thomson](https://twitter.com/importantshock)’s _[An Introduction to Recursion Schemes](http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/)_ and _[Recursion Schemes, Part 2: A Mob of Morphisms](http://patrickthomson.ghost.io/recursion-schemes-part-2/)_.
	public func iterate(transform: Syntax<B, A> -> B) -> B {
		return analysis(
			ifPure: id,
			ifRoll: { transform($0.map { $0.iterate(transform) }) })
	}


	// MARK: Functor

	public func map<C>(@noescape transform: B -> C) -> Free<A, C> {
		return analysis(ifPure: { .Pure(transform($0)) }, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(@noescape transform: B -> Free<A, C>) -> Free<A, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case let .Pure(b):
			return ".Pure(\(String(reflecting: b)))"
		case let .Roll(s):
			return ".Roll(\(String(reflecting: s)))"
		}
	}
}


extension Free where B: PatchConvertible, B.Info == A {
	public typealias Term = Fix<A>

	private func discardNullTerms(syntax: Syntax<Term?, A>) -> Term? {
		switch syntax {
		case let .Leaf(a):
			return .In(.Leaf(a))
		case let .Indexed(a):
			return .In(.Indexed(a.flatMap(id)))
		case let .Keyed(a):
			return .In(.Keyed(Dictionary(elements: a.flatMap { k, v in v.map { (k, $0) } })))
		}
	}

	public var before: Term? {
		return map { $0.patch.state.before }.iterate(self.discardNullTerms)
	}

	public var after: Term? {
		return map { $0.patch.state.after }.iterate(self.discardNullTerms)
	}


	public var inverse: Free {
		return map { B(patch: $0.patch.inverse) }
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
