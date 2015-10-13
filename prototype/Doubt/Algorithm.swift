/// An operation of diffing over terms or collections of terms.
public enum Operation<Recur, A> {
	/// The type of `Term`s over which `Operation`s operate.
	public typealias Term = Fix<A>

	/// The type of `Diff`s which `Operation`s produce.
	public typealias Diff = Free<A, Patch<A>>

	/// Indicates that diffing should compare the enclosed `Term`s.
	///
	/// When run, the enclosed function will be applied to the resulting `Diff`.
	case Recursive(Term, Term, Diff -> Recur)

	/// Represents a diff to be performed on a collection of terms identified by keys.
	case ByKey([String:Term], [String:Term], [String:Diff] -> Recur)

	/// Represents a diff to be performed over an array of terms by index.
	case ByIndex([Term], [Term], [Diff] -> Recur)


	// MARK: Functor

	public func map<Other>(transform: Recur -> Other) -> Operation<Other, A> {
		switch self {
		case let .Recursive(a, b, f):
			return .Recursive(a, b, f >>> transform)
		case let .ByKey(a, b, f):
			return .ByKey(a, b, f >>> transform)
		case let .ByIndex(a, b, f):
			return .ByIndex(a, b, f >>> transform)
		}
	}
}


/// The free monad over `Operation`, implementing the language of diffing.
///
/// As with `Free`, this is “free” in the sense of “unconstrained,” i.e. “the monad induced by `Operation` without extra assumptions.”
///
/// Where `Operation` models a single diffing strategy, `Algorithm` models the recursive selection of diffing strategies at each node. Thus, a value in `Algorithm` models an algorithm for constructing a value in the type `B` from the resulting diffs. By this means, diffing can be adapted not just to the specific grammar, but to specific trees produced by that grammar, and even the values of type `A` encapsulated at each node.
public enum Algorithm<A, B> {
	/// The type of `Term`s over which `Algorithm`s operate.
	public typealias Term = Operation<Algorithm, A>.Term

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Operation<Algorithm, A>.Diff

	/// The injection of a value of type `B` into an `Operation`.
	///
	/// Equally, a way to return a result or throw an error during computation, as determined by the type which `B` is instantiated to, and the specific context in which it is being evaluated.
	case Pure(B)

	/// A recursive instantiation of `Operation`, unrolling another iteration of the recursive type.
	case Roll(Operation<Algorithm, A>)

	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Operation<Algorithm, A> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(a):
			return ifRoll(a)
		}
	}


	// MARK: Functor

	public func map<Other>(transform: B -> Other) -> Algorithm<A, Other> {
		return analysis(ifPure: transform >>> Algorithm<A, Other>.Pure, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(transform: B -> Algorithm<A, C>) -> Algorithm<A, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}


	/// Evaluates the encoded algorithm, returning its result.
	public func evaluate(equals: (A, A) -> Bool, recur: (Term, Term) -> Diff) -> B {
		switch self {
		case let .Pure(b):
			return b

		case let .Roll(.Recursive(a, b, f)):
			// Recur structurally into both terms, if compatible, patching paired sub-terms. This is akin to the shape of unification, except that it computes a patched tree instead of a substitution. It’s also a little like a structural zip on the pair of terms.
			//
			// At the moment, there are no restrictions on whether terms are compatible.
			if Term.equals(equals)(a, b) { return f(Diff(b)).evaluate(equals, recur: recur) }

			switch (a.out, b.out) {
			case let (.Indexed(a), .Indexed(b)) where a.count == b.count:
				return f(.Indexed(zip(a, b).map(recur))).evaluate(equals, recur: recur)

			case let (.Keyed(a), .Keyed(b)) where Array(a.keys) == Array(b.keys):
				return f(.Keyed(Dictionary(elements: b.keys.map { ($0, recur(a[$0]!, b[$0]!)) }))).evaluate(equals, recur: recur)

			default:
				// This must not call `recur` with `a` and `b`, as that would infinite loop if actually recursive.
				return f(Diff.Pure(.Replace(a, b))).evaluate(equals, recur: recur)
			}

		case let .Roll(.ByKey(a, b, f)):
			let recur = {
				Term.equals(equals)($0, $1)
					? Diff($1)
					: recur($0, $1)
			}
			// Essentially [set reconciliation](https://en.wikipedia.org/wiki/Data_synchronization#Unordered_data) on the keys, followed by recurring into the values of the intersecting keys.
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Pure(Patch.Delete(a[$0]!))) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Pure(Patch.Insert(b[$0]!))) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, recur(a[$0]!, b[$0]!)) }
			return f(Dictionary(elements: deleted + inserted + patched)).evaluate(equals, recur: recur)

		case let .Roll(.ByIndex(a, b, f)):
			return f(SES(a, b, equals: equals, recur: recur)).evaluate(equals, recur: recur)
		}
	}
}

extension Algorithm where A: Equatable {
	public func evaluate(recur: (Term, Term) -> Diff) -> B {
		return evaluate(==, recur: recur)
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

extension Algorithm where B: FreeConvertible, B.RollType == A, B.PureType == Patch<A> {
	/// `Algorithm<A, Diff>`s can be constructed from a pair of `Term`s using `ByKey` when `Keyed`, `ByIndex` when `Indexed`, and `Recursive` otherwise.
	public init(_ a: Term, _ b: Term) {
		switch (a.out, b.out) {
		case let (.Keyed(a), .Keyed(b)):
			self = .Roll(.ByKey(a, b, Syntax.Keyed >>> Free.Roll >>> B.init >>> Pure))
		case let (.Indexed(a), .Indexed(b)):
			self = .Roll(.ByIndex(a, b, Syntax.Indexed >>> Free.Roll >>> B.init >>> Pure))
		default:
			self = .Roll(.Recursive(a, b, B.init >>> Algorithm.Pure))
		}
	}

	public func evaluate(equals: (A, A) -> Bool) -> B {
		return evaluate(equals, recur: { Algorithm($0, $1).evaluate(equals).free })
	}
}

extension Algorithm where A: Equatable, B: FreeConvertible, B.RollType == A, B.PureType == Patch<A> {
	public func evaluate() -> B {
		return evaluate(==)
	}
}


import Prelude
