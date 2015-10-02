/// The fixpoint of `Syntax`.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Fix` makes it into a recursive tree by “tying the knot”—each child node of `Syntax<Fix, A>` is represented by a `Fix` which in turn contains a `Syntax<Fix, A>`. So in the same way that the `fix` function allows one to tie a non-recursive function into a recursive one, `Fix` allows one to tie a non-recursive type into a recursive one. Unfortunately, due to Swift’s lack of higher-rank types, this cannot currently be abstracted over the type which is made recursive, and thus it is hard-coded to `Syntax<Fix, A>` rather than provided by a type parameter `F` applied to `Fix<F>`.
public enum Fix<A> {
	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case In(Syntax<Fix, A>)

	public var out: Syntax<Fix, A> {
		switch self {
		case let .In(s):
			return s
		}
	}
}
