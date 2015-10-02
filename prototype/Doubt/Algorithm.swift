/// A language of diffing algorithms.
enum Algorithm<Recur, A> {
	typealias Term = Fix<A>
	typealias Diff = Free<A, Patch<A>>

	case Recursive(Term, Diff -> Recur)
}
