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

	public static func Branch(terms: [Term]) -> Term {
		return Term(.Branch(terms))
	}
}

public enum Syntax<Recur, A>: CustomDebugStringConvertible, CustomDocConvertible {
	case Empty
	case Leaf(A)
	case Branch([Recur])

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return .Leaf(n)
		case let .Branch(vs):
			return .Branch(vs.map(transform))
		}
	}

	public func reduce<T>(initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
		switch self {
		case let .Branch(xs):
			return try xs.reduce(initial, combine: combine)

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
		case let .Branch(vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Branch([ \(s) ])"
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return Doc(n)
		case let .Branch(vs):
			return vs.map(Doc.init).joinWithSeparator(",").bracket("{", "}")
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
		case let .Branch(vs):
			return Hash("Branch", .Ordered(vs.map(recur)))
		}
	}
}

extension Syntax where Recur: Hashable, A: Hashable {
	public var hash: Hash {
		return hash(Hash.init)
	}
}
