/// A `Location` is a structure similar to a `Lens` which allows introspection of & update to subtrees of some tree.
///
/// In the former case, a `Location` is essentially an implementation of a [Zipper][], allowing exploration into and out of nodes, as well as to adjacent nodes.
///
/// [Hinze & Jeuring][] then extended this to support replacing the focused node. `Location` implements their approach (see §5, “A Read-write Web”).
///
/// This is also closely related to [McBride][]’s observations about the derivative of regular types.
///
/// [Zipper]: https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
/// [Hinze & Jeuring]: https://github.com/github/semantic-diff/files/27297/Weaving.a.Web.pdf
/// [McBride]: http://strictlypositive.org/diff.pdf
public struct Location<A>: SequenceType {
	/// Construct a `Location` representing some position within a tree.
	public init(it: A, into: A -> Location?, outOf: A -> Location?, left: A -> Location?, right: A -> Location?) {
		self.it = it
		_left = left
		_right = right
		_outOf = outOf
		_into = into
	}

	/// The node currently in focus.
	public let it: A

	public var into: Location? { return _into(it) }

	public var outOf: Location? { return _outOf(it) }

	public var left: Location? { return _left(it) }

	public var right: Location? { return _right(it) }

	/// The root `Location` in the current exploration.
	public var root: Location {
		return outOf?.root ?? self
	}


	/// Returns the logically next `Location` after the receiver in a pre-order depth-first traversal.
	public var next: Location? {
		return into ?? nextAfter
	}

	/// Returns the logically next `Location` after the receiver and its children in a pre-order depth-first traversal.
	private var nextAfter: Location? {
		return right ?? outOf?.nextAfter
	}


	/// Return a new `Location` by replacing the current value with a new one produced by `f`.
	public func modify(@noescape f: A -> A) -> Location {
		return Location(it: f(it), into: _into, outOf: _outOf, left: _left, right: _right)
	}


	public typealias Weave = A -> Unweave
	public typealias Unweave = (A -> Location?) -> Location?


	// MARK: - Constructors

	public static func nullary(outOf: A -> Location?) -> Location? {
		return nil
	}

	public static func unary(t1: A, _ weave: Weave, _ reconstruct: A -> A)(_ outOf: A -> Location?) -> Location? {
		return Location(flip(weave), { $0[0] } >>> reconstruct >>> outOf, [t1])
	}

	public static func binary(t1: A, _ t2: A, _ weave: Weave, _ reconstruct: (A, A) -> A)(_ outOf: A -> Location?) -> Location? {
		return Location(flip(weave), { ($0[0], $0[1]) } >>> reconstruct >>> outOf, [t1, t2])
	}

	public static func ternary(t1: A, _ t2: A, _ t3: A, _ weave: Weave, _ reconstruct:  (A, A, A) -> A)(_ outOf: A -> Location?) -> Location? {
		return Location(flip(weave), { ($0[0], $0[1], $0[2]) } >>> reconstruct >>> outOf, [t1, t2, t3])
	}

	public static func variadic<C: MutableCollectionType where C.Generator.Element == A, C.Index: BidirectionalIndexType>(ts: C, _ weave: Weave, _ reconstruct: C -> A)(_ outOf: A -> Location?) -> Location? {
		return Location(flip(weave), reconstruct >>> outOf, ts)
	}

	public static func variadic<Key>(ts: [Key:A], _ weave: Weave, _ reconstruct: [Key:A] -> A)(_ outOf: A -> Location?) -> Location? {
		return Location(flip(weave), Dictionary.init >>> reconstruct >>> outOf, Array(ts))
	}

	public static func explore(weave: Weave)(_ a : A) -> Location {
		return Location(it: a, into: flip(weave)(explore(weave) >>> Optional.Some), outOf: const(nil), left: const(nil), right: const(nil))
	}


	// MARK: - Implementation details

	private init?<C: MutableCollectionType where C.Generator.Element == A, C.Index: BidirectionalIndexType>(_ weave: (A -> Location?) -> A -> Location?, _ outOf: C -> Location?, _ ts: C) {
		func update(index: C.Index, _ ts: C)(_ f: C -> Location?)(_ a: A) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			var copy = ts
			copy[index] = a
			return f(copy)
		}
		func into(index: C.Index)(_ ts: C) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			return Location(it: ts[index], into: weave(update(index, ts)(into(index))), outOf: update(index, ts)(outOf), left: update(index, ts)(into(index.predecessor())), right: update(index, ts)(into(index.successor())))
		}
		guard let location = into(ts.startIndex)(ts) else { return nil }
		self = location
	}

	private init?<C: MutableCollectionType, Key where C.Generator.Element == (Key, A), C.Index: BidirectionalIndexType>(_ weave: (A -> Location?) -> A -> Location?, _ outOf: C -> Location?, _ ts: C) {
		func update(index: C.Index, _ ts: C)(_ f: C -> Location?)(_ key: Key)(_ a: A) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			var copy = ts
			copy[index] = (key, a)
			return f(copy)
		}
		func into(index: C.Index)(_ ts: C) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			let (key, value) = ts[index]
			return Location(it: value, into: weave(update(index, ts)(into(index))(key)), outOf: update(index, ts)(outOf)(key), left: update(index, ts)(into(index.predecessor()))(key), right: update(index, ts)(into(index.successor()))(key))
		}
		guard let location = into(ts.startIndex)(ts) else { return nil }
		self = location
	}


	private let _into: A -> Location?
	private let _outOf: A -> Location?
	private let _left: A -> Location?
	private let _right: A -> Location?


	// MARK: SequenceType

	public func generate() -> AnyGenerator<Location> {
		var current: Location? = self
		return anyGenerator {
			let next = current
			current = current?.next
			return next
		}
	}
}


// Flipping of curried functions.
private func flip<A, B, C>(f: A -> B -> C)(_ b: B)(_ a: A) -> C {
	return f(a)(b)
}


import Prelude
