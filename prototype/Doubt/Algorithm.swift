/// The free monad over `Operation`, implementing the language of diffing.
///
/// As with `Free`, this is “free” in the sense of “unconstrained,” i.e. “the monad induced by `Operation` without extra assumptions.”
///
/// Where `Operation` models a single diffing strategy, `Algorithm` models the recursive selection of diffing strategies at each node. Thus, a value in `Algorithm` models an algorithm for constructing a value in the type `Result` from the resulting diffs. By this means, diffing can be adapted not just to the specific grammar, but to specific trees produced by that grammar, and even the values of type `A` encapsulated at each node.
public enum Algorithm<Term: CofreeType, Result> {
	/// The type of `Patch`es produced by `Algorithm`s.
	public typealias Patch = Doubt.Patch<Term>

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Free<Term.Leaf, Term.Annotation, Patch>

	/// The injection of a value of type `Result` into an `Operation`.
	///
	/// Equally, a way to return a result or throw an error during computation, as determined by the type which `Result` is instantiated to, and the specific context in which it is being evaluated.
	case Pure(Result)

	/// A recursive instantiation of `Operation`, unrolling another iteration of the recursive type.
	indirect case Roll(Operation<Algorithm, Term, Diff>)

	public func analysis<C>(@noescape ifPure ifPure: Result -> C, @noescape ifRoll: Operation<Algorithm, Term, Diff> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(a):
			return ifRoll(a)
		}
	}


	// MARK: Functor

	public func map<Other>(transform: Result -> Other) -> Algorithm<Term, Other> {
		return analysis(ifPure: transform >>> Algorithm<Term, Other>.Pure, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(transform: Result -> Algorithm<Term, C>) -> Algorithm<Term, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}
}


import Prelude
