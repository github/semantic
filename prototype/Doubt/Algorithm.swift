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
}
