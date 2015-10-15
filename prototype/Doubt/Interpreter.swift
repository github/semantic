/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: TermType> {
	public typealias Diff = Free<Term.LeafType, Patch<Term>>

	private let equals: (Term, Term) -> Bool

	private func recur(a: Term, _ b: Term) -> Diff? {
		if equals(a, b) { return Diff(b) }

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

		default:
			return nil
		}
	}

	public func run(a: Term, _ b: Term) -> Diff {
		return recur(a, b) ?? .Replace(a, b)
	}
}


import Prelude
