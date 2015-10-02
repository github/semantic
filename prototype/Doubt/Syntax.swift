/// A term in a syntax tree.
///
/// This is a fixpoint of Syntax, essentially enabling it to recur through Term instances.
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
}

public func == <A: Equatable> (left: Term<A>, right: Term<A>) -> Bool {
	return Syntax.equals(ifLeaf: ==, ifRecur: ==)(left.syntax, right.syntax)
}


/// A node in a syntax tree. Expressed algebraically to enable representation of both normal syntax trees and their diffs.
public enum Syntax<Recur, A>: CustomDebugStringConvertible, CustomDocConvertible {
	case Leaf(A)
	case Indexed([Recur])

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case let .Leaf(n):
			return .Leaf(n)
		case let .Indexed(x):
			return .Indexed(x.map(transform))
		}
	}

	// fixme: 🔥
	public func reduce<T>(initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
		switch self {
		case let .Indexed(x):
			return try x.reduce(initial, combine: combine)

		default:
			return initial
		}
	}

	public var debugDescription: String {
		switch self {
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Indexed(x):
			return ".Indexed(\(String(reflecting: x)))"
		}
	}

	public var doc: Doc {
		switch self {
		case let .Leaf(n):
			return Doc(n)
		case let .Indexed(x):
			return x.map(Doc.init).joinWithSeparator(", ").bracket("[", "]")
		}
	}
}


// MARK: - Equality

extension Syntax {
	public static func equals(ifLeaf ifLeaf: (A, A) -> Bool, ifRecur: (Recur, Recur) -> Bool)(_ left: Syntax<Recur, A>, _ right: Syntax<Recur, A>) -> Bool {
		switch (left, right) {
		case let (.Leaf(l1), .Leaf(l2)):
			return ifLeaf(l1, l2)
		case let (.Indexed(v1), .Indexed(v2)):
			return v1.count == v2.count && zip(v1, v2).lazy.map(ifRecur).reduce(true) { $0 && $1 }
		default:
			return false
		}
	}
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return Syntax.equals(ifLeaf: ==, ifRecur: ==)(left, right)
}


extension Term where A: Hashable {
	public var hash: Hash {
		return syntax.hash(ifLeaf: Hash.init, ifRecur: { $0.hash })
	}
}

extension Syntax {
	public func hash(ifLeaf ifLeaf: A -> Hash, ifRecur: Recur -> Hash) -> Hash {
		switch self {
		case let .Leaf(n):
			return Hash("Leaf", ifLeaf(n))
		case let .Indexed(x):
			return Hash("Indexed", .Ordered(x.map(ifRecur)))
		}
	}
}

extension Syntax where Recur: Hashable, A: Hashable {
	public var hash: Hash {
		return hash(ifLeaf: Hash.init, ifRecur: Hash.init)
	}
}
