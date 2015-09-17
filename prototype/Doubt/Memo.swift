public struct Memo<A> {
	public init(unevaluted: () -> A) {
		self.init(.Unevaluated(unevaluted))
	}

	public init(evaluated: A) {
		self.init(.Evaluated(evaluated))
	}

	private init(_ thunk: Thunk<A>) {
		_value = MutableBox(thunk)
	}

	public var value: A {
		return _value.value.value()
	}

	private var _value: MutableBox<Thunk<A>>


	public func map<B>(transform: A -> B) -> Memo<B> {
		return Memo<B> { transform(self.value) }
	}
}

private final class MutableBox<A> {
	init(_ value: A) {
		self.value = value
	}

	var value: A
}

private enum Thunk<A> {
	case Evaluated(A)
	case Unevaluated(() -> A)

	mutating func value() -> A {
		switch self {
		case let .Evaluated(a):
			return a
		case let .Unevaluated(f):
			let a = f()
			self = .Evaluated(a)
			return a
		}
	}
}
