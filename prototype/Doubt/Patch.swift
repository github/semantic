/// A patch to some part of a `Syntax` tree.
public enum Patch<A>: CustomDebugStringConvertible {
	case Replace(Fix<A>, Fix<A>)
	case Insert(Fix<A>)
	case Delete(Fix<A>)

	public var state: (before: Fix<A>?, after: Fix<A>?) {
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
}


// MARK: - Equality

extension Patch {
	public static func equals(param: (A, A) -> Bool)(_ left: Patch, _ right: Patch) -> Bool {
		return Optional.equals(Fix.equals(param))(left.state.before, right.state.before)
			&& Optional.equals(Fix.equals(param))(left.state.after, right.state.after)
	}
}


// MARK: - Hashing

extension Patch {
	public func hash(param: A -> Hash) -> Hash {
		return Hash.Ordered([
			state.before.map { $0.hash(param) } ?? Hash.Empty,
			state.after.map { $0.hash(param) } ?? Hash.Empty
		])
	}
}


/// A hack to enable constrained extensions on `Free<A, Patch<A>>`.
public protocol PatchConvertible {
	typealias Info

	init(patch: Patch<Info>)
	var patch: Patch<Info> { get }
}

extension Patch: PatchConvertible {
	public init(patch: Patch) { self = patch }
	public var patch: Patch { return self }
}
