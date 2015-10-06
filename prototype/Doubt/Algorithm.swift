/// An operation of diffing over terms or collections of terms.
public enum Algorithm<Recur, A> {
	/// The type of `Term`s over which `Algorithm`s operate.
	public typealias Term = Fix<A>

	/// The type of `Diff`s which `Algorithm`s produce.
	public typealias Diff = Free<A, Patch<A>>

	/// Indicates that diffing should compare the enclosed `Term`s.
	///
	/// When run, the enclosed function will be applied to the resulting `Diff`.
	case Recursive(Term, Term, Diff -> Recur)

	/// Represents a diff to be performed on a collection of terms identified by keys.
	case ByKey([String:Term], [String:Term], [String:Diff] -> Recur)

	/// Represents a diff to be performed over an array of terms by index.
	case ByIndex([Term], [Term], [Diff] -> Recur)


	// MARK: Functor

	public func map<Other>(transform: Recur -> Other) -> Algorithm<Other, A> {
		switch self {
		case let .Recursive(a, b, f):
			return .Recursive(a, b, f >>> transform)
		case let .ByKey(a, b, f):
			return .ByKey(a, b, f >>> transform)
		case let .ByIndex(a, b, f):
			return .ByIndex(a, b, f >>> transform)
		}
	}
}


/// The free monad over `Algorithm`, implementing the language of diffing.
///
/// As with `Free`, this is “free” in the sense of “unconstrained,” i.e. “the monad induced by `Algorithm` without extra assumptions.”
///
/// Where `Algorithm` models a single diffing strategy, `FreeAlgorithm` models the recursive selection of diffing strategies at each node. Thus, a value in `FreeAlgorithm` models an algorithm for constructing a value in the type `B` from the resulting diffs. By this means, diffing can be adapted not just to the specific grammar, but to specific trees produced by that grammar, and even the values of type `A` encapsulated at each node.
public enum FreeAlgorithm<A, B> {
	/// The type of `Term`s over which `FreeAlgorithm`s operate.
	public typealias Term = Algorithm<FreeAlgorithm, A>.Term

	/// The type of `Diff`s which `FreeAlgorithm`s produce.
	public typealias Diff = Algorithm<FreeAlgorithm, A>.Diff

	/// The injection of a value of type `B` into an `Algorithm`.
	///
	/// Equally, a way to return a result or throw an error during computation, as determined by the type which `B` is instantiated to, and the specific context in which it is being evaluated.
	case Pure(B)

	/// A recursive instantiation of `Algorithm`, unrolling another iteration of the recursive type.
	case Roll(Algorithm<FreeAlgorithm, A>)

	public func analysis<C>(@noescape ifPure ifPure: B -> C, @noescape ifRoll: Algorithm<FreeAlgorithm, A> -> C) -> C {
		switch self {
		case let .Pure(b):
			return ifPure(b)
		case let .Roll(a):
			return ifRoll(a)
		}
	}


	// MARK: Functor

	public func map<Other>(transform: B -> Other) -> FreeAlgorithm<A, Other> {
		return analysis(ifPure: transform >>> FreeAlgorithm<A, Other>.Pure, ifRoll: { .Roll($0.map { $0.map(transform) }) })
	}


	// MARK: Monad

	public func flatMap<C>(transform: B -> FreeAlgorithm<A, C>) -> FreeAlgorithm<A, C> {
		return analysis(ifPure: transform, ifRoll: { .Roll($0.map { $0.flatMap(transform) }) })
	}


	/// Evaluates the encoded algorithm, returning its result.
	public func evaluate(equals: (A, A) -> Bool, recur: (Term, Term) -> Diff) -> B {
		/// Deep-copies a `Term` into a `Diff` without changes.
		func copy(b: Term) -> Diff {
			return Diff.Roll(b.out.map(copy))
		}

		switch self {
		case let .Pure(b):
			return b

		case let .Roll(.Recursive(a, b, f)):
			return f(Fix.equals(equals)(a, b)
				? copy(b)
				// This must not call `recur` with `a` and `b`, as that would infinite loop if actually recursive.
				: Diff.Pure(.Replace(a, b))).evaluate(equals, recur: recur)

		case let .Roll(.ByKey(a, b, f)):
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Pure(Patch.Delete(a[$0]!))) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Pure(Patch.Insert(b[$0]!))) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, recur(a[$0]!, b[$0]!)) }
			return f(Dictionary(elements: deleted + inserted + patched)).evaluate(equals, recur: recur)

		case let .Roll(.ByIndex(a, b, f)):
			if a.isEmpty { return f(b.map { Diff.Pure(Patch.Insert($0)) }).evaluate(equals, recur: recur) }
			if b.isEmpty { return f(a.map { Diff.Pure(Patch.Delete($0)) }).evaluate(equals, recur: recur) }

			func cost(diff: Diff) -> Int {
				return diff.map(const(1)).iterate { syntax in
					switch syntax {
					case .Leaf:
						return 0
					case let .Indexed(costs):
						return costs.reduce(0, combine: +)
					case let .Keyed(costs):
						return costs.lazy.map { $1 }.reduce(0, combine: +)
					}
				}
			}

			func cons(diff: Diff, rest: Memo<Stream<(Diff, Int)>>) -> Stream<(Diff, Int)> {
				return .Cons((diff, cost(diff) + costOfStream(rest)), rest)
			}

			func costOfStream(stream: Memo<Stream<(Diff, Int)>>) -> Int {
				return stream.value.first?.1 ?? 0
			}

			var matrix: Matrix<Stream<(Diff, Int)>>!
			matrix = Matrix(width: a.count, height: b.count) { i, j in
				let right = matrix[i + 1, j]
				let down = matrix[i, j + 1]
				let diagonal = matrix[i + 1, j + 1]

				let diff = recur(a[i], b[j])

				if let right = right, down = down, diagonal = diagonal {
					// nominate the best edge to continue along
					let best: Memo<Stream<(Diff, Int)>>
					if costOfStream(diagonal) < costOfStream(down) {
						best = costOfStream(diagonal) < costOfStream(right)
							? diagonal
							: right
					} else {
						best = costOfStream(down) < costOfStream(right)
							? down
							: right
					}
					return cons(diff, rest: best)
				}

				// right extent of the edit graph; can only move down
				if let down = down {
					return cons(diff, rest: down)
				}

				// bottom extent of the edit graph; can only move right
				if let right = right {
					return cons(diff, rest: right)
				}

				// bottom-right corner of the edit graph
				return cons(diff, rest: Memo(evaluated: Stream.Nil))
			}

			return f(Array(matrix[0, 0]!.value.map { diff, _ in diff })).evaluate(equals, recur: recur)
		}
	}
}

extension FreeAlgorithm where A: Equatable {
	public func evaluate(recur: (Term, Term) -> Diff) -> B {
		return evaluate(==, recur: recur)
	}
}

/// A hack to work around the unavailability of same-type requirements.
public protocol FreeConvertible {
	typealias RollType
	typealias PureType

	init(free: Free<RollType, PureType>)
	var free: Free<RollType, PureType> { get }
}

extension Free: FreeConvertible {
	public init(free: Free<A, B>) { self = free }
	public var free: Free { return self }
}

extension FreeAlgorithm where B: FreeConvertible, B.RollType == A, B.PureType == Patch<A> {
	/// `FreeAlgorithm<A, Diff>`s can be constructed from a pair of `Term`s using `ByKey` when `Keyed`, `ByIndex` when `Indexed`, and `Recursive` otherwise.
	public init(_ a: Term, _ b: Term) {
		switch (a.out, b.out) {
		case let (.Keyed(a), .Keyed(b)):
			self = .Roll(.ByKey(a, b, Syntax.Keyed >>> Free.Roll >>> B.init >>> Pure))
		case let (.Indexed(a), .Indexed(b)):
			self = .Roll(.ByIndex(a, b, Syntax.Indexed >>> Free.Roll >>> B.init >>> Pure))
		default:
			self = .Roll(.Recursive(a, b, B.init >>> FreeAlgorithm.Pure))
		}
	}

	public func evaluate(equals: (A, A) -> Bool) -> B {
		return evaluate(equals, recur: { FreeAlgorithm($0, $1).evaluate(equals).free })
	}
}

extension FreeAlgorithm where A: Equatable, B: FreeConvertible, B.RollType == A, B.PureType == Patch<A> {
	public func evaluate() -> B {
		return evaluate(==)
	}
}


/// Computes the SES (shortest edit script), i.e. the shortest sequence of diffs (`Free<A, Patch<A>>`) for two arrays of terms (`Fix<A>`) which would suffice to transform `a` into `b`.
///
/// This is computed w.r.t. an `equals` function, which computes the equality of leaf nodes within terms, and a `recur` function, which produces diffs representing matched-up terms.
public func SES<A>(a: [Fix<A>], _ b: [Fix<A>], equals: (A, A) -> Bool, recur: (Fix<A>, Fix<A>) -> Free<A, Patch<A>>) -> [Free<A, Patch<A>>] {
	typealias Term = Fix<A>
	typealias Diff = Free<A, Patch<A>>

	if a.isEmpty { return b.map { Diff.Pure(Patch.Insert($0)) } }
	if b.isEmpty { return a.map { Diff.Pure(Patch.Delete($0)) } }

	func cost(diff: Diff) -> Int {
		return diff.map(const(1)).iterate { syntax in
			switch syntax {
			case .Leaf:
				return 0
			case let .Indexed(costs):
				return costs.reduce(0, combine: +)
			case let .Keyed(costs):
				return costs.lazy.map { $1 }.reduce(0, combine: +)
			}
		}
	}

	func cons(diff: Diff, rest: Memo<Stream<(Diff, Int)>>) -> Stream<(Diff, Int)> {
		return .Cons((diff, cost(diff) + costOfStream(rest)), rest)
	}

	func costOfStream(stream: Memo<Stream<(Diff, Int)>>) -> Int {
		return stream.value.first?.1 ?? 0
	}

	var matrix: Matrix<Stream<(Diff, Int)>>!
	matrix = Matrix(width: a.count, height: b.count) { i, j in
		let right = matrix[i + 1, j]
		let down = matrix[i, j + 1]
		let diagonal = matrix[i + 1, j + 1]

		let diff = recur(a[i], b[j])

		if let right = right, down = down, diagonal = diagonal {
			// nominate the best edge to continue along
			let best: Memo<Stream<(Diff, Int)>>
			if costOfStream(diagonal) < costOfStream(down) {
				best = costOfStream(diagonal) < costOfStream(right)
					? diagonal
					: right
			} else {
				best = costOfStream(down) < costOfStream(right)
					? down
					: right
			}
			return cons(diff, rest: best)
		}

		// right extent of the edit graph; can only move down
		if let down = down {
			return cons(diff, rest: down)
		}

		// bottom extent of the edit graph; can only move right
		if let right = right {
			return cons(diff, rest: right)
		}

		// bottom-right corner of the edit graph
		return cons(diff, rest: Memo(evaluated: Stream.Nil))
	}

	return Array(matrix[0, 0]!.value.map { diff, _ in diff })
}
