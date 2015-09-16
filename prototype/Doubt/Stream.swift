public enum Stream<A> {
	case Nil
	case Cons(A, () -> Stream)

	public var uncons: (first: A, rest: Stream)? {
		switch self {
		case let .Cons(first, rest):
			return (first, rest())
		default:
			return nil
		}
	}

	public var first: A? {
		return uncons?.first
	}

	public var rest: Stream {
		return uncons?.rest ?? .Nil
	}
}
