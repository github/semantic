/// A node in a syntax tree. Expressed algebraically to enable representation of both normal syntax trees and their diffs.
public enum Syntax<Recur, A>: CustomDebugStringConvertible {
	case Leaf(A)
	case Indexed([Recur])
	case Fixed([Recur])
	case Keyed([String:Recur])


	// MARK: Functor

	public func map<T>(@noescape transform: Recur throws -> T) rethrows -> Syntax<T, A> {
		switch self {
		case let .Leaf(n):
			return .Leaf(n)
		case let .Indexed(x):
			return try .Indexed(x.map(transform))
		case let .Fixed(x):
			return try .Fixed(x.map(transform))
		case let .Keyed(d):
			return try .Keyed(Dictionary(elements: d.map { try ($0, transform($1)) }))
		}
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Indexed(x):
			return ".Indexed(\(String(reflecting: x)))"
		case let .Fixed(x):
			return ".Fixed(\(String(reflecting: x)))"
		case let .Keyed(d):
			return ".Keyed(\(String(reflecting: d)))"
		}
	}
}


// MARK: - Hylomorphism

/// Hylomorphism through `Syntax`.
///
/// A hylomorphism (from the Aristotelian philosophy that form and matter are one) is a function of type `A → B` whose call-tree is linear in the size of the nodes produced by `up`. Conceptually, it’s the composition of a catamorphism (see also `cata`) and an anamorphism (see also `ana`), but is implemented by [Stream fusion](http://lambda-the-ultimate.org/node/2192) and as such enjoys O(n) time complexity, O(1) size complexity, and small constant factors for both (modulo inadvisable implementations of `up` and `down`).
///
/// Hylomorphisms are used to construct diffs corresponding to equal terms; see also `CofreeType.zip`.
///
/// `hylo` can be used with arbitrary functors which can eliminate to and introduce with `Syntax` values.
public func hylo<A, B, Leaf>(down: Syntax<B, Leaf> -> B, _ up: A -> Syntax<A, Leaf>)(_ a: A) -> B {
	return down(up(a).map(hylo(down, up)))
}

/// Reiteration through `Syntax`.
///
/// This is a form of hylomorphism (from the Aristotelian philosophy that form and matter are one). As such, it returns a function of type `A → B` whose call-tree is linear in the size of the nodes produced by `up`. Conceptually, it’s the composition of a catamorphism (see also `cata`) and an anamorphism (see also `ana`), but is implemented by [Stream fusion](http://lambda-the-ultimate.org/node/2192) and as such enjoys O(n) time complexity, O(1) size complexity, and small constant factors for both (modulo inadvisable implementations of `up` and `down`).
///
/// Hylomorphisms are used to construct diffs corresponding to equal terms; see also `CofreeType.zip`.
///
/// `hylo` can be used with arbitrary functors which can eliminate to and introduce with `Annotation` & `Syntax` pairs.
public func hylo<A, B, Leaf, Annotation>(down: (Annotation, Syntax<B, Leaf>) -> B, _ up: A -> (Annotation, Syntax<A, Leaf>)) -> A -> B {
	return up >>> { ($0, $1.map(hylo(down, up))) } >>> down
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
	public static func equals(leaf leaf: (A, A) -> Bool, recur: (Recur, Recur) -> Bool)(_ left: Syntax<Recur, A>, _ right: Syntax<Recur, A>) -> Bool {
		switch (left, right) {
		case let (.Leaf(l1), .Leaf(l2)):
			return leaf(l1, l2)
		case let (.Indexed(v1), .Indexed(v2)):
			return v1.count == v2.count && zip(v1, v2).lazy.map(recur).reduce(true) { $0 && $1 }
		case let (.Fixed(v1), .Fixed(v2)):
			return v1.count == v2.count && zip(v1, v2).lazy.map(recur).reduce(true) { $0 && $1 }
		case let (.Keyed(d1), .Keyed(d2)):
			return Set(d1.keys) == Set(d2.keys) && d1.keys.map { recur(d1[$0]!, d2[$0]!) }.reduce(true) { $0 && $1 }
		default:
			return false
		}
	}
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return Syntax.equals(leaf: ==, recur: ==)(left, right)
}


// MARK: - JSON

extension Syntax {
	public func JSON(@noescape leaf leaf: A -> Doubt.JSON, @noescape recur: Recur -> Doubt.JSON) -> Doubt.JSON {
		switch self {
		case let .Leaf(a):
			return [ "leaf": leaf(a) ]
		case let .Indexed(a):
			return [ "indexed": .Array(a.map(recur)) ]
		case let .Fixed(a):
			return [ "fixed": .Array(a.map(recur)) ]
		case let .Keyed(d):
			return [ "keyed": .Dictionary(Dictionary(elements: d.map { ($0, recur($1)) })) ]
		}
	}
}


import Prelude
