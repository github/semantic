/// An operation of diffing over terms or collections of terms.
public enum Algorithm<Recur, A> {
	/// The type of `Term`s over which `Algorithm`s operate.
	public typealias Term = Fix<A>

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Free<A, Patch<A>>

	/// Indicates that diffing should compare the enclosed `Term`s.
	///
	/// When run, the enclosed function will be applied to the resulting `Diff`.
	case Recursive(Term, Term, Diff -> Recur)

	/// Represents a diff to be performed on a collection of terms identified by keys.
	case ByKey([String:Term], [String:Term], [String:Diff] -> Recur)
	// fixme: SES 😰
//	case ByIndex([Term], [Term], [Diff] -> Recur)


	// MARK: Functor

	public func map<Other>(transform: Recur -> Other) -> Algorithm<Other, A> {
		switch self {
		case let .Recursive(a, b, f):
			return .Recursive(a, b, f >>> transform)
		case let .ByKey(a, b, f):
			return .ByKey(a, b, f >>> transform)
		}
	}
}


// MARK: - Running

extension Algorithm {
	/// Evaluates the encoded algorithm, returning its result.
	public func evaluate(equals: (A, A) -> Bool) -> Recur {
		/// Deep-copies a `Term` into a `Diff` without changes.
		func copy(b: Term) -> Diff {
			return Diff.Roll(b.out.map(copy))
		}

		switch self {
		case let .Recursive(a, b, f):
			return f(Fix.equals(equals)(a, b)
				? copy(b)
				: Diff.Pure(.Replace(a, b)))

		case let .ByKey(a, b, f):
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Pure(Patch.Delete(a[$0]!))) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Pure(Patch.Insert(b[$0]!))) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, Diff.Pure(Patch.Replace(a[$0]!, b[$0]!))) }
			return f(Dictionary(elements: deleted + inserted + patched))
		}
	}
}


/// The free monad over `Algorithm`, implementing the language of diffing.
///
/// As with `Free`, this is “free” in the sense of “unconstrained,” i.e. “the monad induced by `Algorithm` without extra assumptions.”
///
/// Where `Algorithm` models a single diffing strategy, `FreeAlgorithm` models the recursive selection of diffing strategies at each node. Thus, a value in `FreeAlgorithm` models an algorithm for constructing a value in the type `B` from the resulting diffs. By this means, diffing can be adapted not just to the specific grammar, but to specific trees produced by that grammar, and even the values of type `A` encapsulated at each node.
public enum FreeAlgorithm<A, B> {
	/// The injection of a value of type `B` into an `Algorithm`.
	///
	/// Equally, a way to return a result or throw an error during computation, as determined by the type which `B` is instantiated to, and the specific context in which it is being evaluated.
	case Pure(B)

	/// A recursive instantiation of `Algorithm`, unrolling another iteration of the recursive type.
	case Roll(Algorithm<FreeAlgorithm, A>)

	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Algorithm<FreeAlgorithm, A> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(a):
			return ifRoll(a)
		}
	}


	// MARK: Functor

	public func map<Other>(transform: B -> Other) -> FreeAlgorithm<A, Other> {
		return analysis(ifPure: transform >>> FreeAlgorithm<A, Other>.Pure, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(transform: B -> FreeAlgorithm<A, C>) -> FreeAlgorithm<A, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}


	/// Evaluates the encoded algorithm, returning its result.
	public func evaluate(equals: (A, A) -> Bool) -> B {
		switch self {
		case let .Pure(b):
			return b

		case let .Roll(r):
			return r.evaluate(equals).evaluate(equals)
		}
	}
}

extension FreeAlgorithm where A: Equatable {
	public func evaluate() -> B {
		return evaluate(==)
	}
}

/// A hack to work around the unavailability of same-type requirements.
public protocol FreeConvertible {
	typealias RollType
	typealias PureType

	init(free: Free<RollType, PureType>)
	var free: Free<RollType, PureType> { get }
}

extension Free: FreeConvertible {
	public init(free: Free<A, B>) { self = free }
	public var free: Free { return self }
}

extension FreeAlgorithm where B: FreeConvertible, B.RollType == A, B.PureType == Patch<A> {
	public init(_ a: Fix<A>, _ b: Fix<A>) {
		switch (a.out, b.out) {
		case let (.Keyed(a), .Keyed(b)):
			self = .Roll(.ByKey(a, b, Syntax.Keyed >>> Free.Roll >>> B.init >>> FreeAlgorithm.Pure))
		default:
			self = .Roll(.Recursive(a, b, B.init >>> FreeAlgorithm.Pure))
		}
	}
}
