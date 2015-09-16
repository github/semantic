public enum Stream<A> {
	case Nil
	case Cons(A, () -> Stream)

	public init<S: SequenceType where S.Generator.Element == A>(sequence: S) {
		self = Stream(generator: sequence.generate())
	}

	public init<G: GeneratorType where G.Element == A>(var generator: G) {
		self = Stream { generator.next() }
	}

	public init(_ f: () -> A?) {
		self = f().map { Stream.Cons($0, { Stream(f) }) } ?? Stream.Nil
	}

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
