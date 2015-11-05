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
	indirect case Roll(Annotation, Syntax<Free, Leaf>)


	/// Construct a `Free` from a `CofreeType` with matching `Leaf` and `Annotation` types, copying the recursive structure of the term in via hylomorphism.
	///
	/// The resulting `Free` value will not have any `Pure` cases.
	public init<Term: CofreeType where Term.Leaf == Leaf, Term.Annotation == Annotation>(_ term: Term) {
		self = hylo(Free.Roll, Term.eliminate)(term)
	}


	/// Reduce the receiver by iteration.
	///
	/// `Pure` values are simply unpacked. `Roll` values are mapped recursively, and then have `transform` applied to them.
	///
	/// This forms a _catamorphism_ (from the Greek “cata”, “downwards”; compare “catastrophe”), a generalization of folds over regular trees (and datatypes isomorphic to them). It operates at the leaves first, and then branches near the periphery, recursively collapsing values by whatever is computed by `transform`. Catamorphisms are themselves an example of _recursion schemes_, which characterize specific well-behaved patterns of recursion. This gives `cata` some useful properties for computations performed over trees.
	///
	/// Due to the character of recursion captured by catamorphisms, `cata` ensures that computation will not only halt, but will further be linear in the size of the receiver. (Nesting a call to `cata` will therefore result in O(n²) complexity.) This guarantee is achieved by careful composition of calls to `map` with recursive calls to `cata`, only calling `transform` once the recursive call has completed. `transform` is itself non-recursive, receiving a `Syntax` whose recurrences have already been flattened to `Value`.
	///
	/// The linearity of `cata` in the size of the receiver makes it trivial to compute said size, by counting leaves as 1 and summing branches’ children:
	///
	///		func size<Leaf, Annotation, Value>(free: Free<Leaf, Annotation, Value>) -> Int {
	///			return free.cata { flattenedSyntax in
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
	/// While not every function on a given `Free` can be computed using `cata`, these guarantees of termination and complexity, as well as the brevity and focus on the operation being performed n times, make it a desirable scaffolding for any function which can.
	///
	/// For a lucid, in-depth tutorial on recursion schemes, I recommend [Patrick Thomson](https://twitter.com/importantshock)’s _[An Introduction to Recursion Schemes](http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/)_ and _[Recursion Schemes, Part 2: A Mob of Morphisms](http://patrickthomson.ghost.io/recursion-schemes-part-2/)_.
	public func cata(@noescape transform: (Annotation, Syntax<Value, Leaf>) throws -> Value) rethrows -> Value {
		switch self {
		case let .Pure(a):
			return a
		case let .Roll(annotation, syntax):
			return try transform(annotation, syntax.map { try $0.cata(transform) })
		}
	}


	/// Reduces the receiver top-down, left-to-right, starting from an `initial` value, and applying `combine` to successive values.
	public func reduce(initial: Value, @noescape combine: (Value, Value) -> Value) -> Value {
		return cata {
			switch $1 {
			case .Leaf:
				return initial
			case let .Indexed(a):
				return a.reduce(initial, combine: combine)
			case let .Fixed(a):
				return a.reduce(initial, combine: combine)
			case let .Keyed(a):
				return a.values.reduce(initial, combine: combine)
			}
		}
	}

	/// Returns a function which sums `Free`s by first `transform`ing `Pure` values into integers, and then summing these.
	public static func sum(@noescape transform: Value -> Int)(_ free: Free) -> Int {
		return free.map(transform).reduce(0, combine: +)
	}


	// MARK: Functor

	public func map<C>(@noescape transform: Value throws -> C) rethrows -> Free<Leaf, Annotation, C> {
		switch self {
		case let .Pure(a):
			return try .Pure(transform(a))
		case let .Roll(annotation, syntax):
			return try .Roll(annotation, syntax.map { try $0.map(transform) })
		}
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
	public static func Introduce(annotation: Annotation)(syntax: Syntax<Free, Leaf>) -> Free {
		return Roll(annotation, syntax)
	}

	/// Anamorphism over `Free`.
	///
	/// Unfolds a tree bottom-up by recursively applying `transform` to a series of values starting with `seed`. Since `Syntax.Leaf` does not recur, this will halt when it has produced leaves for every branch.
	public static func ana(@noescape unfold: Annotation throws -> Syntax<Annotation, Leaf>)(_ seed: Annotation) rethrows -> Free {
		return try Roll(seed, unfold(seed).map { try ana(unfold)($0) })
	}
}


extension Free where Value: PatchType, Value.Element == Cofree<Leaf, ()> {
	public typealias Term = Value.Element

	public func merge(@noescape transform: Value -> Term) -> Term {
		return map(transform).cata { Cofree((), $1) }
	}

	public func merge(@noescape transform: Value -> Term?) -> Term? {
		return map(transform).cata { Free.discardNullTerms($1) }
	}

	private static func discardNullTerms(syntax: Syntax<Term?, Leaf>) -> Term? {
		switch syntax {
		case let .Leaf(a):
			return Cofree((), .Leaf(a))
		case let .Indexed(a):
			return Cofree((), .Indexed(a.flatMap(id)))
		case let .Fixed(a):
			return Cofree((), .Fixed(a.flatMap(id)))
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
	public static func equals(pure pure: (Value, Value) -> Bool, leaf: (Leaf, Leaf) -> Bool, annotation: (Annotation, Annotation) -> Bool)(_ left: Free, _ right: Free) -> Bool {
		switch (left, right) {
		case let (.Pure(a), .Pure(b)):
			return pure(a, b)
		case let (.Roll(annotation1, syntax1), .Roll(annotation2, syntax2)):
			return annotation(annotation1, annotation2) && Syntax.equals(leaf: leaf, recur: equals(pure: pure, leaf: leaf, annotation: annotation))(syntax1, syntax2)
		default:
			return false
		}
	}
}

public func == <Leaf: Equatable, Value: Equatable, Annotation: Equatable> (left: Free<Leaf, Annotation, Value>, right: Free<Leaf, Annotation, Value>) -> Bool {
	return Free.equals(pure: ==, leaf: ==, annotation: ==)(left, right)
}

public func == <Term: CofreeType, Annotation: Equatable where Term.Leaf: Equatable> (left: Free<Term.Leaf, Annotation, Patch<Term>>, right: Free<Term.Leaf, Annotation, Patch<Term>>) -> Bool {
	return Free.equals(pure: Patch.equals(Term.equals(==)), leaf: ==, annotation: ==)(left, right)
}

public func == <Term: CofreeType, Annotation where Term.Leaf: Equatable> (left: Free<Term.Leaf, Annotation, Patch<Term>>, right: Free<Term.Leaf, Annotation, Patch<Term>>) -> Bool {
	return Free.equals(pure: Patch.equals(Term.equals(==)), leaf: ==, annotation: const(true))(left, right)
}


// MARK: - JSON

extension Free {
	public func JSON(pure pure: Value -> Doubt.JSON, leaf: Leaf -> Doubt.JSON, annotation: Annotation -> Doubt.JSON) -> Doubt.JSON {
		switch self {
		case let .Pure(a):
			return [ "pure": pure(a) ]
		case let .Roll(a, b):
			return [
				"roll": [
					"extract": annotation(a),
					"unwrap": b.JSON(leaf: leaf, recur: { $0.JSON(pure: pure, leaf: leaf, annotation: annotation) })
				]
			]
		}
	}
}


// MARK: - Weaving

extension Free {
	public func explore() -> Location<Free> {
		func weave(free: Free) -> Location<Free>.Unweave {
			switch free {
			case .Pure, .Roll(_, .Leaf):
				return Location.nullary

			case let .Roll(annotation, .Indexed(i)):
				return Location.variadic(i, weave, { Free.Roll(annotation, .Indexed($0)) })

			case let .Roll(annotation, .Fixed(f)):
				return Location.variadic(f, weave, { Free.Roll(annotation, .Fixed($0)) })

			case let .Roll(annotation, .Keyed(k)):
				return Location.variadic(k, weave, { Free.Roll(annotation, .Keyed($0)) })
			}
		}
		return Location.explore(weave)(self)
	}
}


import Prelude
