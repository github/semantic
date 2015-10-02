public enum Free<A, B> {
	case Pure(B)
	indirect case Roll(Syntax<Free, A>)
}
