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
	case Branch(Recur)

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
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
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Branch(x):
			return ".Branch(\(String(reflecting: x)))"
		}
	}

	public var doc: Doc {
		switch self {
		case let .Leaf(n):
			return Doc(n)
		case let .Branch(x):
			return Doc(x)
		}
	}
}


// MARK: - Equality

extension Syntax {
	public static func equals(ifLeaf ifLeaf: (A, A) -> Bool, ifRecur: (Recur, Recur) -> Bool)(_ left: Syntax<Recur, A>, _ right: Syntax<Recur, A>) -> Bool {
		switch (left, right) {
		case let (.Leaf(l1), .Leaf(l2)):
			return ifLeaf(l1, l2)
		case let (.Branch(v1), .Branch(v2)):
			return ifRecur(v1, v2)
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
		return syntax.hash { $0.hash }
	}
}

extension Syntax where A: Hashable {
	public func hash(recur: Recur -> Hash) -> Hash {
		switch self {
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
