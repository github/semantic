/// A language of diffing algorithms.
public enum Algorithm<Recur, A> {
	/// The type of `Term`s over which `Algorithm`s operate.
	public typealias Term = Fix<A>

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Free<A, Patch<A>>

	/// Indicates that diffing should compare the enclosed `Term`s.
	///
	/// When run, the enclosed function will be applied to the resulting `Diff`.
	case Recursive(Term, Term, Diff -> Recur)


	// MARK: Functor

	public func map<Other>(transform: Recur -> Other) -> Algorithm<Other, A> {
		switch self {
		case let .Recursive(a, b, f):
			return .Recursive(a, b, f >>> transform)
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
			if Fix.equals(equals)(a, b) { return f(copy(b)) }

			switch (a.out, b.out) {
			default:
				return f(Diff.Pure(.Replace(a, b)))
			}
		}
	}
}
