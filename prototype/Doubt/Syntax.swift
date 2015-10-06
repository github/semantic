/// A node in a syntax tree. Expressed algebraically to enable representation of both normal syntax trees and their diffs.
public enum Syntax<Recur, A>: CustomDebugStringConvertible, CustomDocConvertible {
	case Leaf(A)
	case Indexed([Recur])
	case Keyed([String:Recur])


	// MARK: Functor

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case let .Leaf(n):
			return .Leaf(n)
		case let .Indexed(x):
			return .Indexed(x.map(transform))
		case let .Keyed(d):
			return .Keyed(Dictionary(elements: d.map { ($0, transform($1)) }))
		}
	}

	// fixme: 🔥
	public func reduce<T>(initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
		switch self {
		case let .Indexed(x):
			return try x.reduce(initial, combine: combine)

		case let .Keyed(d):
			return try d.lazy.map { $1 }.reduce(initial, combine: combine)

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
		case let .Keyed(d):
			return ".Keyed(\(String(reflecting: d)))"
		}
	}

	public var doc: Doc {
		switch self {
		case let .Leaf(n):
			return Doc(n)
		case let .Indexed(x):
			return x.map(Doc.init).joinWithSeparator(", ").bracket("[", "]")
		case let .Keyed(d):
			return d.lazy.map { Doc($0) <> Doc(":") <+> Doc($1) }.joinWithSeparator(", ").bracket("[", "]")
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
		case let (.Keyed(d1), .Keyed(d2)):
			return Array(d1.keys) == Array(d2.keys) && d1.keys.lazy.map { ifRecur(d1[$0]!, d2[$0]!) }.reduce(true) { $0 && $1 }
		default:
			return false
		}
	}
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return Syntax.equals(ifLeaf: ==, ifRecur: ==)(left, right)
}


extension Syntax {
	public func hash(ifLeaf ifLeaf: A -> Hash, ifRecur: Recur -> Hash) -> Hash {
		switch self {
		case let .Leaf(n):
			return Hash("Leaf", ifLeaf(n))
		case let .Indexed(x):
			return Hash("Indexed", .Ordered(x.map(ifRecur)))
		case let .Keyed(d):
			return Hash("Keyed", .Ordered(d.keys.sort().map { Hash($0, ifRecur(d[$0]!)) }))
		}
	}
}

extension Syntax where Recur: Hashable, A: Hashable {
	public var hash: Hash {
		return hash(ifLeaf: Hash.init, ifRecur: Hash.init)
	}
}
