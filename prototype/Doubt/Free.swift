/// The free monad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the monad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `Pure` case) to satisfy the monad laws.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Free` makes it recursive through the `Roll` case, and allows it to wrap values of type `B` through the `Pure` case.
///
/// In Doubt, this allows us to represent diffs as values of the `Free` monad obtained from `Syntax`, injecting `Patch` into the tree; or otherwise put, a diff is a tree of mutually-recursive `Free.Roll`/`Syntax` nodes with `Pure` nodes injecting the actual changes.
public enum Free<A, B>: CustomDebugStringConvertible, SyntaxConvertible {
	/// The injection of a value of type `B` into the `Syntax` tree.
	case Pure(B)

	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case Roll(Syntax<Free, A>)


	/// Recursively copies a `Term: TermType where Term.LeafType == A` into a `Free<A, B>`, essentially mapping `Term.unwrap` onto `Free.Roll`.
	public init<Term: TermType where Term.LeafType == A>(_ term: Term) {
		self = .Roll(term.unwrap.map(Free.init))
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


	/// Reduces the receiver top-down, left-to-right, starting from an `initial` value, and applying `combine` to successive values.
	public func reduce(initial: B, combine: (B, B) -> B) -> B {
		return iterate {
			switch $0 {
			case .Leaf:
				return initial
			case let .Indexed(a):
				return a.reduce(initial, combine: combine)
			case let .Keyed(a):
				return a.values.reduce(initial, combine: combine)
			}
		}
	}

	/// Returns a function which sums `Free`s by first `transform`ing `Pure` values into integers, and then summing these.
	public static func sum(transform: B -> Int)(_ free: Free) -> Int {
		return free.map(transform).reduce(0, combine: +)
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


	// MARK: SyntaxConvertible

	public init(syntax: Syntax<Free, A>) {
		self = .Roll(syntax)
	}
}


extension Free where B: PatchType, B.Element == Cofree<A, ()> {
	public typealias Term = B.Element

	private func discardNullTerms(syntax: Syntax<Term?, A>) -> Term? {
		switch syntax {
		case let .Leaf(a):
			return Cofree((), .Leaf(a))
		case let .Indexed(a):
			return Cofree((), .Indexed(a.flatMap(id)))
		case let .Keyed(a):
			return Cofree((), .Keyed(Dictionary(elements: a.flatMap { k, v in v.map { (k, $0) } })))
		}
	}

	public var before: Term? {
		return map { $0.state.before }.iterate(self.discardNullTerms)
	}

	public var after: Term? {
		return map { $0.state.after }.iterate(self.discardNullTerms)
	}
}


// MARK: - Patch construction

extension Free where B: PatchType {
	public static func Replace(before: B.Element, _ after: B.Element) -> Free {
		return .Pure(B(replacing: before, with: after))
	}

	public static func Insert(after: B.Element) -> Free {
		return .Pure(B(inserting: after))
	}

	public static func Delete(before: B.Element) -> Free {
		return .Pure(B(deleting: before))
	}


	public var inverse: Free {
		return map { $0.inverse }
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

public func == <Term: TermType where Term.LeafType: Equatable> (left: Free<Term.LeafType, Patch<Term>>, right: Free<Term.LeafType, Patch<Term>>) -> Bool {
	return Free.equals(ifPure: Patch.equals(Term.equals(==)), ifRoll: ==)(left, right)
}


// MARK: - JSON

extension Free {
	public func JSON(ifPure ifPure: B -> Doubt.JSON, ifLeaf: A -> Doubt.JSON) -> Doubt.JSON {
		return analysis(
			ifPure: ifPure,
			ifRoll: {
				$0.JSON(ifLeaf: ifLeaf, ifRecur: { $0.JSON(ifPure: ifPure, ifLeaf: ifLeaf) })
			})
	}
}

extension Free where A: CustomJSONConvertible {
	public func JSON(ifPure: B -> Doubt.JSON) -> Doubt.JSON {
		return JSON(ifPure: ifPure, ifLeaf: { $0.JSON })
	}
}


import Prelude
