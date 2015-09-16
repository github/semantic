public enum Stream<A>: SequenceType {
	case Nil
	case Cons(A, Memo<Stream>)

	public init<S: SequenceType where S.Generator.Element == A>(sequence: S) {
		self = Stream(generator: sequence.generate())
	}

	public init<G: GeneratorType where G.Element == A>(var generator: G) {
		self = Stream { generator.next() }
	}

	public init(_ f: () -> A?) {
		self = f().map { Stream.Cons($0, Memo { Stream(f) }) } ?? Stream.Nil
	}

	public var uncons: (first: A, rest: Stream)? {
		switch self {
		case let .Cons(first, rest):
			return (first, rest.value)
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

	public var isEmpty: Bool {
		return uncons == nil
	}


	public func map<B>(transform: A -> B) -> Stream<B> {
		return uncons.map { first, rest in Stream<B>.Cons(transform(first), Memo { rest.map(transform) }) } ?? Stream<B>.Nil
	}


	public func generate() -> AnyGenerator<A> {
		var current = self
		return anyGenerator {
			let next = current.first
			current = current.rest
			return next
		}
	}
}
