/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: TermType> {
	/// The type of diffs constructed by `Interpreter`s.
	public typealias Diff = Free<Term.LeafType, Patch<Term>>

	public init(equals: (Term, Term) -> Bool, comparable: (Term, Term) -> Bool, cost: Diff -> Int) {
		self.equals = equals
		self.comparable = comparable
		self.cost = cost
	}

	private let equals: (Term, Term) -> Bool
	private let comparable: (Term, Term) -> Bool
	private let cost: Diff -> Int

	/// Diff `a` against `b`, if comparable.
	private func recur(a: Term, _ b: Term) -> Diff? {
		if equals(a, b) { return Diff(b) }
		guard comparable(a, b) else { return nil }

		let algorithm: Algorithm<Term, Diff>
		switch (a.unwrap, b.unwrap) {
		case let (.Keyed(a), .Keyed(b)):
			algorithm = .Roll(.ByKey(a, b, Syntax.Keyed >>> Diff.Roll >>> Algorithm.Pure))
		case let (.Indexed(a), .Indexed(b)):
			algorithm = .Roll(.ByIndex(a, b, Syntax.Indexed >>> Diff.Roll >>> Algorithm.Pure))
		default:
			algorithm = .Roll(.Recursive(a, b, Algorithm.Pure))
		}
		return recur(algorithm)
	}

	private func recur(algorithm: Algorithm<Term, Diff>) -> Diff? {
		switch algorithm {
		case let .Pure(diff):
			return diff

		case let .Roll(.Recursive(a, b, f)):
			// Recur structurally into both terms, patching differing sub-terms. This is akin to unification, except that it computes a patched tree instead of a substitution. It’s also a little like a structural zip on pairs of terms.
			switch (a.unwrap, b.unwrap) {
			case let (.Indexed(a), .Indexed(b)) where a.count == b.count:
				return recur(f(.Indexed(zip(a, b).map(run))))

			case let (.Keyed(a), .Keyed(b)) where Array(a.keys) == Array(b.keys):
				return recur(f(.Keyed(Dictionary(elements: b.keys.map { ($0, self.run(a[$0]!, b[$0]!)) }))))

			default:
				// This must not call `recur` directly with `a` and `b`, as that would infinite loop if actually recursive.
				return recur(f(.Replace(a, b)))
			}

		case let .Roll(.ByKey(a, b, f)):
			// Perform [set reconciliation](https://en.wikipedia.org/wiki/Data_synchronization#Unordered_data) on the keys, followed by recurring into the values of the intersecting keys.
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Delete(a[$0]!)) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Insert(b[$0]!)) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, run(a[$0]!, b[$0]!)) }
			return recur(f(Dictionary(elements: deleted + inserted + patched)))

		case let .Roll(.ByIndex(a, b, f)):
			return recur(f(SES(a, b, cost: cost, recur: recur)))
		}
	}

	public func run(a: Term, _ b: Term) -> Diff {
		return recur(a, b) ?? .Replace(a, b)
	}
}


import Prelude
