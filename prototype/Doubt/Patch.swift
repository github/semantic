/// A patch to some part of a `Syntax` tree.
public enum Patch<A> {
	case Replace(Fix<A>?, Fix<A>?)

	public var replace: (before: Fix<A>?, after: Fix<A>?) {
		switch self {
		case let .Replace(a, b):
			return (a, b)
		}
	}
}
