/// A patch to some part of a `Syntax` tree.
public enum Patch<A> {
	case Replace(Fix<A>?, Fix<A>?)
}
