public enum Stream<A>: NilLiteralConvertible, SequenceType {
	case Nil
	indirect case Cons(A, Memo<Stream>)

	public init<S: SequenceType where S.Generator.Element == A>(sequence: S) {
		self = Stream(generator: sequence.generate())
	}

	public init<G: GeneratorType where G.Element == A>(var generator: G) {
		self = Stream { generator.next() }
	}

	public init(_ f: () -> A?) {
		self = f().map { Stream.Cons($0, Memo { Stream(f) }) } ?? Stream.Nil
	}

	public func analysis<B>(@noescape ifCons ifCons: (A, Memo<Stream>) -> B, @noescape ifNil: () -> B) -> B {
		switch self {
		case let .Cons(first, rest):
			return ifCons(first, rest)
		case .Nil:
			return ifNil()
		}
	}

	public var uncons: (first: A, rest: Memo<Stream>)? {
		return analysis(ifCons: { $0 }, ifNil: { nil })
	}

	public var first: A? {
		return uncons?.first
	}

	public var rest: Memo<Stream> {
		return analysis(ifCons: { $1 }, ifNil: { Memo(evaluated: .Nil) })
	}

	public var isEmpty: Bool {
		return uncons == nil
	}


	public func map<B>(transform: A -> B) -> Stream<B> {
		return uncons.map { first, rest in Stream<B>.Cons(transform(first), Memo { rest.map(transform) }) } ?? Stream<B>.Nil
	}


	public func concat(other: Stream) -> Stream {
		switch self {
		case .Nil:
			return other
		case let .Cons(first, rest):
			return .Cons(first, rest.map { $0.concat(other) })
		}
	}


	public init(nilLiteral: ()) {
		self = .Nil
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
