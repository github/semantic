/// The free monad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the monad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `Pure` case) to satisfy the monad laws.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Free` makes it recursive through the `Roll` case, and allows it to wrap values of type `Value` through the `Pure` case.
///
/// In Doubt, this allows us to represent diffs as values of the `Free` monad obtained from `Syntax`, injecting `Patch` into the tree; or otherwise put, a diff is a tree of mutually-recursive `Free.Roll`/`Syntax` nodes with `Pure` nodes injecting the actual changes.
public enum Free<Leaf, Annotation, Value>: CustomDebugStringConvertible {
	/// The injection of a value of type `Value` into the `Syntax` tree.
	case Pure(Value)

	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case Roll(Syntax<Free, Leaf>)


	/// Recursively copies a `Term: TermType where Term.Leaf == Leaf, Term.Annotation == Annotation` into a `Free<Leaf, Annotation, Value>`, essentially mapping `Term.unwrap` onto `Free.Roll`.
	public init<Term: CofreeType where Term.Leaf == Leaf>(_ term: Term) {
		self = .Roll(term.unwrap.map(Free.init))
	}


	public func analysis<C>(@noescape ifPure ifPure: Value -> C, @noescape ifRoll: Syntax<Free, Leaf> -> C) -> C {
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
	/// Due to the character of recursion captured by catamorphisms, `iterate` ensures that computation will not only halt, but will further be linear in the size of the receiver. (Nesting a call to `iterate` will therefore result in O(n²) complexity.) This guarantee is achieved by careful composition of calls to `map` with recursive calls to `iterate`, only calling `transform` once the recursive call has completed. `transform` is itself non-recursive, receiving a `Syntax` whose recurrences have already been flattened to `Value`.
	///
	/// The linearity of `iterate` in the size of the receiver makes it trivial to compute said size, by counting leaves as 1 and summing branches’ children:
	///
	///		func size<Leaf, Annotation, Value>(free: Free<Leaf, Annotation, Value>) -> Int {
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
	public func iterate(transform: Syntax<Value, Leaf> -> Value) -> Value {
		return analysis(
			ifPure: id,
			ifRoll: { $0.map { $0.iterate(transform) } } >>> transform)
	}


	/// Reduces the receiver top-down, left-to-right, starting from an `initial` value, and applying `combine` to successive values.
	public func reduce(initial: Value, combine: (Value, Value) -> Value) -> Value {
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
	public static func sum(transform: Value -> Int)(_ free: Free) -> Int {
		return free.map(transform).reduce(0, combine: +)
	}


	// MARK: Functor

	public func map<C>(@noescape transform: Value -> C) -> Free<Leaf, Annotation, C> {
		return analysis(ifPure: { .Pure(transform($0)) }, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(@noescape transform: Value -> Free<Leaf, Annotation, C>) -> Free<Leaf, Annotation, C> {
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


// MARK: - Anamorphism

extension Free {
	/// Anamorphism over `Free`.
	///
	/// Unfolds a tree bottom-up by recursively applying `transform` to a series of values starting with `seed`. Since `Syntax.Leaf` does not recur, this will halt when it has produced leaves for every branch.
	public static func ana<Seed>(unfold: Seed -> Syntax<Seed, Leaf>)(_ seed: Seed) -> Free {
		return (Roll <<< { $0.map(ana(unfold)) } <<< unfold) <| seed
	}
}


// MARK: - Hylomorphism

public func hylo<A, B, Leaf>(down: Syntax<B, Leaf> -> B, _ up: A -> Syntax<A, Leaf>) -> A -> B {
	return up >>> { $0.map(hylo(down, up)) } >>> down
}


extension Free where Value: PatchType, Value.Element == Cofree<Leaf, ()> {
	public typealias Term = Value.Element

	public func merge(transform: Value -> Term) -> Term {
		return map(transform).iterate { Cofree((), $0) }
	}

	public func merge(transform: Value -> Term?) -> Term? {
		return map(transform).iterate(Free.discardNullTerms)
	}

	private static func discardNullTerms(syntax: Syntax<Term?, Leaf>) -> Term? {
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
		return merge { $0.state.before }
	}

	public var after: Term? {
		return merge { $0.state.after }
	}
}


// MARK: - Patch construction

extension Free where Value: PatchType {
	public static func Replace(before: Value.Element, _ after: Value.Element) -> Free {
		return .Pure(Value(replacing: before, with: after))
	}

	public static func Insert(after: Value.Element) -> Free {
		return .Pure(Value(inserting: after))
	}

	public static func Delete(before: Value.Element) -> Free {
		return .Pure(Value(deleting: before))
	}


	public var inverse: Free {
		return map { $0.inverse }
	}
}


// MARK: - Equality

extension Free {
	public static func equals(pure pure: (Value, Value) -> Bool, leaf: (Leaf, Leaf) -> Bool)(_ left: Free, _ right: Free) -> Bool {
		switch (left, right) {
		case let (.Pure(a), .Pure(b)):
			return pure(a, b)
		case let (.Roll(a), .Roll(b)):
			return Syntax.equals(leaf: leaf, recur: equals(pure: pure, leaf: leaf))(a, b)
		default:
			return false
		}
	}
}

public func == <Leaf: Equatable, Value: Equatable, Annotation> (left: Free<Leaf, Annotation, Value>, right: Free<Leaf, Annotation, Value>) -> Bool {
	return Free.equals(pure: ==, leaf: ==)(left, right)
}

public func == <Term: CofreeType, Annotation where Term.Leaf: Equatable> (left: Free<Term.Leaf, Annotation, Patch<Term>>, right: Free<Term.Leaf, Annotation, Patch<Term>>) -> Bool {
	return Free.equals(pure: Patch.equals(Term.equals(==)), leaf: ==)(left, right)
}


// MARK: - JSON

extension Free {
	public func JSON(pure pure: Value -> Doubt.JSON, leaf: Leaf -> Doubt.JSON) -> Doubt.JSON {
		return analysis(
			ifPure: pure,
			ifRoll: {
				$0.JSON(leaf: leaf, recur: { $0.JSON(pure: pure, leaf: leaf) })
			})
	}
}


import Prelude
