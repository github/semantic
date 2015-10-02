/// A patch to some part of a `Syntax` tree.
public enum Patch<A> {
	case Replace(Fix<A>?, Fix<A>?)

	public var state: (before: Fix<A>?, after: Fix<A>?) {
		switch self {
		case let .Replace(a, b):
			return (a, b)
		}
	}
}


// MARK: - Equality

extension Patch {
	public static func equals(param: (A, A) -> Bool)(_ left: Patch, _ right: Patch) -> Bool {
		return Optional.equals(Fix.equals(param))(left.state.before, right.state.before)
			&& Optional.equals(Fix.equals(param))(left.state.after, right.state.after)
	}
}
