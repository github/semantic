public enum Term<A: Equatable>: CustomDebugStringConvertible, CustomDocConvertible, CustomStringConvertible, Equatable {
	public init(_ out: Syntax<Term, A>) {
		self = .Roll(out)
	}

	indirect case Roll(Syntax<Term, A>)

	public var syntax: Syntax<Term, A> {
		switch self {
		case let .Roll(syntax):
			return syntax
		}
	}

	public var debugDescription: String {
		return syntax.debugDescription
	}

	public var doc: Doc {
		return syntax.doc
	}


	public static var Empty: Term {
		return Term(.Empty)
	}

	public static func Leaf(a: A) -> Term {
		return Term(.Leaf(a))
	}

	public static func Branch(term: Term) -> Term {
		return Term(.Branch(term))
	}
}

public enum Syntax<Recur, A>: CustomDebugStringConvertible, CustomDocConvertible {
	case Empty
	case Leaf(A)
	case Branch(Recur)

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return .Leaf(n)
		case let .Branch(x):
			return .Branch(transform(x))
		}
	}

	public func reduce<T>(initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
		switch self {
		case let .Branch(x):
			return try combine(initial, x)

		default:
			return initial
		}
	}

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Branch(x):
			return ".Branch(\(String(reflecting: x)))"
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return Doc(n)
		case let .Branch(x):
			return Doc(x)
		}
	}
}

extension Term where A: Hashable {
	public var hash: Hash {
		return syntax.hash { $0.hash }
	}
}

extension Syntax where A: Hashable {
	public func hash(recur: Recur -> Hash) -> Hash {
		switch self {
		case .Empty:
			return Hash("Empty")
		case let .Leaf(n):
			return Hash("Leaf", Hash(n))
		case let .Branch(x):
			return Hash("Branch", recur(x))
		}
	}
}

extension Syntax where Recur: Hashable, A: Hashable {
	public var hash: Hash {
		return hash(Hash.init)
	}
}
