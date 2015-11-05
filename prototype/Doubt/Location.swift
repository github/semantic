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
	public init(it: A, down: A -> Location?, up: A -> Location?, left: A -> Location?, right: A -> Location?) {
		self.it = it
		_left = left
		_right = right
		_up = up
		_down = down
	}

	/// The node currently in focus.
	public let it: A

	public var down: Location? { return _down(it) }

	public var up: Location? { return _up(it) }

	public var left: Location? { return _left(it) }

	public var right: Location? { return _right(it) }

	/// The root `Location` in the current exploration.
	public var root: Location {
		return up?.root ?? self
	}


	/// Returns the logically next `Location` after the receiver in a pre-order depth-first traversal.
	public var next: Location? {
		return down ?? nextAfter
	}

	/// Returns the logically next `Location` after the receiver and its children in a pre-order depth-first traversal.
	private var nextAfter: Location? {
		return right ?? up?.nextAfter
	}


	/// Return a new `Location` by replacing the current value with a new one produced by `f`.
	public func modify(@noescape f: A -> A) -> Location {
		return Location(it: f(it), down: _down, up: _up, left: _left, right: _right)
	}


	public typealias Weave = A -> Unweave
	public typealias Unweave = (A -> Location?) -> Location?


	// MARK: - Constructors

	public static func nullary(up: A -> Location?) -> Location? {
		return nil
	}

	public static func unary(t1: A, _ weave: Weave, _ reconstruct: A -> A)(_ up: A -> Location?) -> Location? {
		return Location(flip(weave), reconstruct >>> up, t1)
	}

	public static func binary(t1: A, _ t2: A, _ weave: Weave, _ reconstruct: (A, A) -> A)(_ up: A -> Location?) -> Location? {
		return Location(flip(weave), reconstruct >>> up, t1, t2)
	}

	public static func ternary(t1: A, _ t2: A, _ t3: A, _ weave: Weave, _ reconstruct:  (A, A, A) -> A)(_ up: A -> Location?) -> Location? {
		return Location(flip(weave), reconstruct >>> up, t1, t2, t3)
	}

	public static func variadic<C: MutableCollectionType where C.Generator.Element == A, C.Index: BidirectionalIndexType>(ts: C, _ weave: Weave, _ reconstruct: C -> A)(_ up: A -> Location?) -> Location? {
		return Location(flip(weave), reconstruct >>> up, ts)
	}

	public static func variadic<Key>(ts: [Key:A], _ weave: Weave, _ reconstruct: [Key:A] -> A)(_ up: A -> Location?) -> Location? {
		return Location(flip(weave), Dictionary.init >>> reconstruct >>> up, Array(ts))
	}

	public static func explore(weave: Weave)(_ a : A) -> Location {
		return Location(it: a, down: flip(weave)(explore(weave) >>> Optional.Some), up: const(nil), left: const(nil), right: const(nil))
	}


	// MARK: - Implementation details

	private init?<C: MutableCollectionType where C.Generator.Element == A, C.Index: BidirectionalIndexType>(_ weave: (A -> Location?) -> A -> Location?, _ up: C -> Location?, _ ts: C) {
		func update(index: C.Index)(_ f: C -> Location?)(_ a: A) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			var copy = ts
			copy[index] = a
			return f(copy)
		}
		func into(index: C.Index)(_ ts: C) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			return Location(it: ts[index], down: weave(update(index)(into(index))), up: update(index)(up), left: update(index)(into(index.predecessor())), right: update(index)(into(index.successor())))
		}
		guard let location = into(ts.startIndex)(ts) else { return nil }
		self = location
	}

	private init?<C: MutableCollectionType, Key where C.Generator.Element == (Key, A), C.Index: BidirectionalIndexType>(_ weave: (A -> Location?) -> A -> Location?, _ up: C -> Location?, _ ts: C) {
		func update(index: C.Index)(_ f: C.Index -> C -> Location?)(_ key: Key)(_ a: A) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			var copy = ts
			copy[index] = (key, a)
			return f(index)(copy)
		}
		func into(index: C.Index)(_ ts: C) -> Location? {
			guard ts.indices.contains(index) else { return nil }
			let (key, value) = ts[index]
			return Location(it: value, down: weave(update(index)(into)(key)), up: update(index)(const(up))(key), left: update(index.predecessor())(into)(key), right: update(index.successor())(into)(key))
		}
		guard let location = into(ts.startIndex)(ts) else { return nil }
		self = location
	}

	private init?(_ weave: (A -> Location?) -> A -> Location?, _ up: A -> Location?, _ a: A) {
		func into(t1: A) -> Location? {
			return Location(it: t1, down: weave(into), up: up, left: const(nil), right: const(nil))
		}
		guard let location = into(a) else { return nil }
		self = location
	}

	private init?(_ weave: (A -> Location?) -> A -> Location?, _ up: (A, A) -> Location?, _ t1: A, _ t2: A) {
		func into1(t1: A, _ t2: A) -> Location? {
			let update: ((A, A) -> Location?) -> A -> Location? = { fl in { t1 in fl(t1, t2) } }
			return Location(it: t1, down: weave(update(into1)), up: update(up), left: const(nil), right: update(into2))
		}
		func into2(t1: A, _ t2: A) -> Location? {
			let update: ((A, A) -> Location?) -> A -> Location? = { fl in { t2 in fl(t1, t2) } }
			return Location(it: t2, down: weave(update(into2)), up: update(up), left: update(into1), right: const(nil))
		}
		guard let location = into1(t1, t2) else { return nil }
		self = location
	}

	private init?(_ weave: (A -> Location?) -> A -> Location?, _ up: (A, A, A) -> Location?, _ t1: A, _ t2: A, _ t3: A) {
		func into1(t1: A, _ t2: A, _ t3: A) -> Location? {
			let update: ((A, A, A) -> Location?) -> A -> Location? = { fl in { t1 in fl(t1, t2, t3) } }
			return Location(it: t1, down: weave(update(into1)), up: update(up), left: const(nil), right: update(into2))
		}
		func into2(t1: A, _ t2: A, _ t3: A) -> Location? {
			let update: ((A, A, A) -> Location?) -> A -> Location? = { fl in { t2 in fl(t1, t2, t3) } }
			return Location(it: t1, down: weave(update(into2)), up: update(up), left: update(into1), right: update(into3))
		}
		func into3(t1: A, _ t2: A, _ t3: A) -> Location? {
			let update: ((A, A, A) -> Location?) -> A -> Location? = { fl in { t3 in fl(t1, t2, t3) } }
			return Location(it: t1, down: weave(update(into3)), up: update(up), left: update(into2), right: const(nil))
		}
		guard let location = into1(t1, t2, t3) else { return nil }
		self = location
	}


	private let _down: A -> Location?
	private let _up: A -> Location?
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
