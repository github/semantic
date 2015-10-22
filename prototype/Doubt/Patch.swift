/// A patch to some part of a `Syntax` tree.
public enum Patch<A>: CustomDebugStringConvertible {
	case Replace(A, A)
	case Insert(A)
	case Delete(A)

	public var state: (before: A?, after: A?) {
		switch self {
		case let .Replace(a, b):
			return (a, b)
		case let .Insert(b):
			return (nil, b)
		case let .Delete(a):
			return (a, nil)
		}
	}


	public var inverse: Patch {
		switch self {
		case let .Replace(a, b):
			return .Replace(b, a)
		case let .Insert(b):
			return .Delete(b)
		case let .Delete(a):
			return .Insert(a)
		}
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case let .Replace(a, b):
			return ".Replace(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Insert(b):
			return ".Insert(\(String(reflecting: b)))"
		case let .Delete(a):
			return ".Delete(\(String(reflecting: a)))"
		}
	}
}


// MARK: - Functor

extension Patch {
	public func map<B>(@noescape transform: A -> B) -> Patch<B> {
		switch self {
		case let .Replace(a, b):
			return .Replace(transform(a), transform(b))
		case let .Delete(a):
			return .Delete(transform(a))
		case let .Insert(b):
			return .Insert(transform(b))
		}
	}
}


// MARK: - Equality

extension Patch {
	public static func equals(param: (A, A) -> Bool)(_ left: Patch, _ right: Patch) -> Bool {
		return Optional.equals(param)(left.state.before, right.state.before)
			&& Optional.equals(param)(left.state.after, right.state.after)
	}
}


// MARK: - Cost calculations

extension Patch {
	/// Returns a function which computes the size of a `patch` as the sum of the sizes of its terms, as computed by `size`.
	public static func sum(size: A -> Int)(_ patch: Patch) -> Int {
		return (patch.state.before.map(size) ?? 0) + (patch.state.after.map(size) ?? 0)
	}

	/// Returns a function which computes the size of a `patch` as the absolute difference of the sizes of its terms, as computed by `size`.
	public static func difference(size: A -> Int)(_ patch: Patch) -> Int {
		return abs((patch.state.before.map(size) ?? 0) - (patch.state.after.map(size) ?? 0))
	}
}

extension Patch where A: TermType {
	/// Computes the size of a `patch` as the sum of the sizes of its terms.
	public static func sum(patch: Patch) -> Int {
		return sum { $0.size } (patch)
	}

	/// Computes the size of a `patch` as the absolute difference of the sizes of its terms.
	public static func difference(patch: Patch) -> Int {
		return difference { $0.size } (patch)
	}
}


// MARK: - JSON

extension Patch {
	public func JSON(ifLeaf: A -> Doubt.JSON) -> Doubt.JSON {
		switch self {
		case let .Replace(a, b):
			return [
				"replace": [
					"before": ifLeaf(a),
					"after": ifLeaf(b),
				]
			]
		case let .Insert(b):
			return [
				"insert": ifLeaf(b),
			]
		case let .Delete(a):
			return [
				"delete": ifLeaf(a)
			]
		}
	}
}


extension Patch where A: CustomJSONConvertible {
	public var JSON: Doubt.JSON {
		return JSON { $0.JSON }
	}
}


// MARK: - PatchType

/// A hack to enable constrained extensions on `Free<Leaf, Patch<Term: TermType where Term.Leaf == Leaf>`.
public protocol PatchType {
	typealias Element

	var state: (before: Element?, after: Element?) { get }

	var inverse: Self { get }

	init(replacing before: Element, with after: Element)
	init(deleting before: Element)
	init(inserting after: Element)
}

extension Patch: PatchType {
	public init(replacing before: A, with after: A) {
		self = .Replace(before, after)
	}

	public init(deleting before: A) {
		self = .Delete(before)
	}

	public init(inserting after: A) {
		self = .Insert(after)
	}
}


extension PatchType where Element: CofreeType, Element.Annotation == Range<String.Index> {
	public func JSON(a a: String, b: String) -> Doubt.JSON {
		return [
			"before": state.before.map { $0.JSON(a) } ?? nil,
			"after": state.after.map { $0.JSON(b) } ?? nil,
		]
	}
}
