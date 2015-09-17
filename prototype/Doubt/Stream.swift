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
		return analysis(
			ifCons: { .Cons(transform($0), $1.map { $0.map(transform) }) },
			ifNil: const(nil))
	}

	public func flatMap<B>(transform: A -> Stream<B>) -> Stream<B> {
		return analysis(
			ifCons: { transform($0).concat($1.map { $0.flatMap(transform) }) },
			ifNil: const(nil))
	}

	public func concat(other: Memo<Stream>) -> Stream {
		return analysis(
			ifCons: { .Cons($0, $1.map { $0.concat(other.value) }) },
			ifNil: { other.value })
	}

	public func concat(other: Stream) -> Stream {
		return concat(Memo(evaluated: other))
	}


	public init(nilLiteral: ()) {
		self = .Nil
	}


	public func generate() -> AnyGenerator<A> {
		var current = Memo(evaluated: self)
		return anyGenerator {
			let next = current.value.first
			current = current.value.rest
			return next
		}
	}
}
