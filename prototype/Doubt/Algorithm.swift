/// The free monad over `Operation`, implementing the language of diffing.
///
/// As with `Free`, this is “free” in the sense of “unconstrained,” i.e. “the monad induced by `Operation` without extra assumptions.”
///
/// Where `Operation` models a single diffing strategy, `Algorithm` models the recursive selection of diffing strategies at each node. Thus, a value in `Algorithm` models an algorithm for constructing a value in the type `B` from the resulting diffs. By this means, diffing can be adapted not just to the specific grammar, but to specific trees produced by that grammar, and even the values of type `A` encapsulated at each node.
public enum Algorithm<Term: TermType, B> {
	/// The type of `Patch`es produced by `Algorithm`s.
	public typealias Patch = Doubt.Patch<Term>

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Free<Term.LeafType, Patch>

	/// The injection of a value of type `B` into an `Operation`.
	///
	/// Equally, a way to return a result or throw an error during computation, as determined by the type which `B` is instantiated to, and the specific context in which it is being evaluated.
	case Pure(B)

	/// A recursive instantiation of `Operation`, unrolling another iteration of the recursive type.
	indirect case Roll(Operation<Algorithm, Term, Diff>)

	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Operation<Algorithm, Term, Diff> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(a):
			return ifRoll(a)
		}
	}


	// MARK: Functor

	public func map<Other>(transform: B -> Other) -> Algorithm<Term, Other> {
		return analysis(ifPure: transform >>> Algorithm<Term, Other>.Pure, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(transform: B -> Algorithm<Term, C>) -> Algorithm<Term, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}


	/// Evaluates the encoded algorithm, returning its result.
	public func evaluate(equals: (Term, Term) -> Bool, recur: (Term, Term) -> Diff?) -> B {
		let recur = {
			equals($0, $1)
				? Diff($1)
				: recur($0, $1)
		}
		let recurOrReplace = {
			recur($0, $1) ?? .Pure(.Replace($0, $1))
		}
		switch self {
		case let .Pure(b):
			return b

		case let .Roll(.Recursive(a, b, f)):
			// Recur structurally into both terms, if compatible, patching paired sub-terms. This is akin to the shape of unification, except that it computes a patched tree instead of a substitution. It’s also a little like a structural zip on the pair of terms.
			//
			// At the moment, there are no restrictions on whether terms are compatible.
			if equals(a, b) { return f(Diff(b)).evaluate(equals, recur: recur) }

			switch (a.out, b.out) {
			case let (.Indexed(a), .Indexed(b)) where a.count == b.count:
				return f(.Indexed(zip(a, b).map(recurOrReplace))).evaluate(equals, recur: recur)

			case let (.Keyed(a), .Keyed(b)) where Array(a.keys) == Array(b.keys):
				return f(.Keyed(Dictionary(elements: b.keys.map { ($0, recurOrReplace(a[$0]!, b[$0]!)) }))).evaluate(equals, recur: recur)

			default:
				// This must not call `recur` with `a` and `b`, as that would infinite loop if actually recursive.
				return f(Diff.Pure(.Replace(a, b))).evaluate(equals, recur: recur)
			}

		case let .Roll(.ByKey(a, b, f)):
			// Essentially [set reconciliation](https://en.wikipedia.org/wiki/Data_synchronization#Unordered_data) on the keys, followed by recurring into the values of the intersecting keys.
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Pure(Patch.Delete(a[$0]!))) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Pure(Patch.Insert(b[$0]!))) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, recurOrReplace(a[$0]!, b[$0]!)) }
			return f(Dictionary(elements: deleted + inserted + patched)).evaluate(equals, recur: recur)

		case let .Roll(.ByIndex(a, b, f)):
			return f(SES(a, b, recur: recur)).evaluate(equals, recur: recur)
		}
	}
}

extension Algorithm where Term: Equatable {
	public func evaluate(recur: (Term, Term) -> Diff?) -> B {
		return evaluate(==, recur: recur)
	}
}

extension Algorithm where B: FreeConvertible, B.RollType == Term.LeafType, B.PureType == Algorithm<Term, B>.Patch {
	/// `Algorithm<Term, Diff>`s can be constructed from a pair of `Term`s using `ByKey` when `Keyed`, `ByIndex` when `Indexed`, and `Recursive` otherwise.
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

	public func evaluate(equals: (Term, Term) -> Bool) -> B {
		return evaluate(equals, recur: { Algorithm($0, $1).evaluate(equals).free })
	}

	public func evaluate<C>(equals: (Term, Term) -> Bool, categorize: Term -> Set<C>) -> B {
		return evaluate(equals, recur: {
			(categorize($0).isEmpty || categorize($1).isEmpty) && !categorize($0).intersect(categorize($1)).isEmpty
				? Algorithm($0, $1).evaluate(equals).free
				: nil
		})
	}
}

extension Algorithm where Term: Equatable, B: FreeConvertible, B.RollType == Term.LeafType, B.PureType == Algorithm<Term, B>.Patch {
	public func evaluate() -> B {
		return evaluate(==)
	}
}

extension Algorithm where Term: Categorizable, B: FreeConvertible, B.RollType == Term.LeafType, B.PureType == Algorithm<Term, B>.Patch {
	public func evaluate(equals: (Term, Term) -> Bool) -> B {
		return evaluate(equals, categorize: { $0.categories })
	}
}


extension Algorithm where Term: Categorizable, Term: Equatable, B: FreeConvertible, B.RollType == Term.LeafType, B.PureType == Algorithm<Term, B>.Patch {
	public func evaluate() -> B {
		return evaluate(==, categorize: { $0.categories })
	}
}


import Prelude
