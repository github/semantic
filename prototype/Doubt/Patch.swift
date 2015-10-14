/// A patch to some part of a `Syntax` tree.
public enum Patch<A>: CustomDebugStringConvertible, CustomDocConvertible {
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


	/// The cost of a patch to the diff.
	public var cost: Int {
		switch self {
		case .Replace:
			return 2
		default:
			return 1
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


	// MARK: CustomDocConvertible

	public var doc: Doc {
		return (state.before.map(Doc.init)?.bracket("{-", "-}") ?? .Empty)
			<> (state.after.map(Doc.init)?.bracket("{+", "+}") ?? .Empty)
	}
}


// MARK: - Equality

extension Patch {
	public static func equals(param: (A, A) -> Bool)(_ left: Patch, _ right: Patch) -> Bool {
		return Optional.equals(param)(left.state.before, right.state.before)
			&& Optional.equals(param)(left.state.after, right.state.after)
	}
}


// MARK: - Hashing

extension Patch {
	public func hash(param: A -> Hash) -> Hash {
		return Hash.Ordered([
			state.before.map(param) ?? Hash.Empty,
			state.after.map(param) ?? Hash.Empty
		])
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


/// A hack to enable constrained extensions on `Free<A, Patch<Fix<A>>`.
public protocol PatchConvertible {
	typealias Element

	init(patch: Patch<Element>)
	var patch: Patch<Element> { get }
}

extension Patch: PatchConvertible {
	public init(patch: Patch) { self = patch }
	public var patch: Patch { return self }
}
