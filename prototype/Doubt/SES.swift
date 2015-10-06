//  Copyright © 2015 GitHub. All rights reserved.

/// Computes the SES (shortest edit script), i.e. the shortest sequence of diffs (`Free<A, Patch<A>>`) for two arrays of terms (`Fix<A>`) which would suffice to transform `a` into `b`.
///
/// This is computed w.r.t. an `equals` function, which computes the equality of leaf nodes within terms, and a `recur` function, which produces diffs representing matched-up terms.
public func SES<A>(a: [Fix<A>], _ b: [Fix<A>], equals: (A, A) -> Bool, recur: (Fix<A>, Fix<A>) -> Free<A, Patch<A>>) -> [Free<A, Patch<A>>] {
	typealias Term = Fix<A>
	typealias Diff = Free<A, Patch<A>>

	if a.isEmpty { return b.map { Diff.Pure(Patch.Insert($0)) } }
	if b.isEmpty { return a.map { Diff.Pure(Patch.Delete($0)) } }

	func cost(diff: Diff) -> Int {
		return diff.map { $0.cost }.iterate { syntax in
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

	// A matrix whose values are streams representing paths through the edit graph, carrying both the diff & the cost of the remainder of the path.
	var matrix: Matrix<Stream<(Diff, Int)>>!
	matrix = Matrix(width: a.count, height: b.count) { i, j in
		let right = matrix[i + 1, j]
		let down = matrix[i, j + 1]
		let diagonal = matrix[i + 1, j + 1]

		func copy(term: Term) -> Diff {
			return Diff.Roll(term.out.map(copy))
		}
		let recur = {
			Fix.equals(equals)($0, $1)
				? copy($1)
				: recur($0, $1)
		}

		if let right = right, down = down, diagonal = diagonal {
			let costs = (right: costOfStream(right), down: costOfStream(down), diagonal: costOfStream(diagonal))
			// nominate the best edge to continue along
			let best: Memo<Stream<(Diff, Int)>>
			let diff: Diff
			if costs.diagonal < costs.down {
				(best, diff) = costs.diagonal < costs.right
					? (diagonal, recur(a[i], b[j]))
					: (right, Diff.Pure(Patch.Delete(a[i])))
			} else {
				(best, diff) = costs.down < costs.right
					? (down, Diff.Pure(Patch.Insert(b[j])))
					: (right, Diff.Pure(Patch.Delete(a[i])))
			}
			return cons(diff, rest: best)
		}

		// right extent of the edit graph; can only move down
		if let down = down {
			return cons(Diff.Pure(Patch.Insert(b[j])), rest: down)
		}

		// bottom extent of the edit graph; can only move right
		if let right = right {
			return cons(Diff.Pure(Patch.Delete(a[i])), rest: right)
		}

		// bottom-right corner of the edit graph
		return cons(recur(a[i], b[j]), rest: Memo(evaluated: Stream.Nil))
	}

	return Array(matrix[0, 0]!.value.map { diff, _ in diff })
}
