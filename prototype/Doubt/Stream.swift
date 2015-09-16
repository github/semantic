public enum Stream<A> {
	case Nil
	case Cons(A, () -> Stream)
}
