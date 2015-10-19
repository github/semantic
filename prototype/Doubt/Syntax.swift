/// A node in a syntax tree. Expressed algebraically to enable representation of both normal syntax trees and their diffs.
public enum Syntax<Recur, A>: CustomDebugStringConvertible {
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
}


// MARK: - ArrayLiteralConvertible

extension Syntax: ArrayLiteralConvertible {
	public init(arrayLiteral: Recur...) {
		self = .Indexed(arrayLiteral)
	}
}


// MARK: - DictionaryLiteralConvertible

extension Syntax: DictionaryLiteralConvertible {
	public init(dictionaryLiteral elements: (String, Recur)...) {
		self = .Keyed(Dictionary(elements: elements))
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
			return Set(d1.keys) == Set(d2.keys) && d1.keys.map { ifRecur(d1[$0]!, d2[$0]!) }.reduce(true) { $0 && $1 }
		default:
			return false
		}
	}
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return Syntax.equals(ifLeaf: ==, ifRecur: ==)(left, right)
}


// MARK: - JSON

extension Syntax {
	public func JSON(@noescape ifLeaf ifLeaf: A -> Doubt.JSON, @noescape ifRecur: Recur -> Doubt.JSON) -> Doubt.JSON {
		switch self {
		case let .Leaf(a):
			return ifLeaf(a)
		case let .Indexed(a):
			return .Array(a.map(ifRecur))
		case let .Keyed(d):
			return .Dictionary(Dictionary(elements: d.map { ($0, ifRecur($1)) }))
		}
	}
}


// MARK: - Construction

/// SyntaxConvertible types can be constructed with the same constructors available on Syntax itself, as a convenience.
public protocol SyntaxConvertible {
	typealias RecurType
	typealias LeafType

	init(syntax: Syntax<RecurType, LeafType>)
}

extension SyntaxConvertible {
	public static func Leaf(value: LeafType) -> Self {
		return Self(syntax: .Leaf(value))
	}

	public static func Indexed(children: [RecurType]) -> Self {
		return Self(syntax: .Indexed(children))
	}

	public static func Keyed(children: [String:RecurType]) -> Self {
		return Self(syntax: .Keyed(children))
	}
}
