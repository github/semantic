/// Computes the SES (shortest edit script), i.e. the shortest sequence of diffs (`Free<Leaf, Annotation, Patch<Term>>`) for two arrays of `Term`s which would suffice to transform `a` into `b`.
///
/// This is computed w.r.t. an `equals` function, which computes the equality of leaf nodes within terms, and a `recur` function, which produces diffs representing matched-up terms.
public func SES<Leaf, Annotation, C: CollectionType>(a: C, _ b: C, cost: Free<Leaf, Annotation, Patch<C.Generator.Element>> -> Int, recur: (C.Generator.Element, C.Generator.Element) -> Free<Leaf, Annotation, Patch<C.Generator.Element>>?) -> [Free<Leaf, Annotation, Patch<C.Generator.Element>>] {
	typealias Diff = Free<Leaf, Annotation, Patch<C.Generator.Element>>

	if a.isEmpty { return b.map { .Insert($0) } }
	if b.isEmpty { return a.map { .Delete($0) } }

	// A matrix whose values are streams representing paths through the edit graph, carrying both the diff & the cost of the remainder of the path.
	var matrix: Matrix<Stream<(Diff, Int)>, C.Index>!
	matrix = Matrix(across: a.startIndex..<a.endIndex.successor(), down: b.startIndex..<b.endIndex.successor()) { i, j in
		// Some explanation is warranted:
		//
		// 1. `matrix` captures itself during construction, because each vertex in the edit graph depends on other vertices. This is safe, because a) `Matrix` populates its fields lazily, and b) vertices only depend on those vertices downwards and rightwards of them.
		//
		// 2. `matrix` is sized bigger than `a.count` x `b.count`. This is safe, because a) we only get a[i]/b[j] when right/down are non-nil (respectively), and b) right/down are found by looking up elements (i + 1, j) & (i, j + 1) in the matrix, which returns `nil` when out of bounds. So we only access a[i] and b[j] when i and j are in bounds.

		let right = matrix[i.successor(), j]
		let down = matrix[i, j.successor()]
		let diagonal = matrix[i.successor(), j.successor()]

		if let diagonal = diagonal, right = right, down = down {
			let here = recur(a[i], b[j])
			if let diagonalDiff = here {
				let hereCost = cost(diagonalDiff)
				let diagonalCost = hereCost + (diagonal.value.first?.1 ?? 0)
				// If the diff at this vertex is zero-cost, we’re not going to find a cheaper one either rightwards or downwards. We can therefore short-circuit selecting the best outgoing edge and save ourselves evaluating the entire row rightwards and the entire column downwards from this point.
				//
				// Thus, in the best case (two equal sequences), we now complete in O(n + m). However, this optimization only applies to equalities at the beginning of the edit graph; once inequalities are encountered, the remainder of the diff is effectively O(nm).
				guard hereCost != 0 else { return .Cons((diagonalDiff, diagonalCost), diagonal) }

				let rightDiff = Diff.Delete(a[i])
				let rightCost = cost(rightDiff) + (right.value.first?.1 ?? 0)
				let downDiff = Diff.Insert(b[j])
				let downCost = cost(downDiff) + (down.value.first?.1 ?? 0)

				if rightCost < downCost && rightCost < diagonalCost {
					return .Cons((rightDiff, rightCost), right)
				} else if downCost < diagonalCost {
					return .Cons((downDiff, downCost), down)
				} else {
					return .Cons((diagonalDiff, diagonalCost), diagonal)
				}
			} else {
				let rightDiff = Diff.Delete(a[i])
				let rightCost = cost(rightDiff) + (right.value.first?.1 ?? 0)
				let downDiff = Diff.Insert(b[j])
				let downCost = cost(downDiff) + (down.value.first?.1 ?? 0)

				if rightCost < downCost {
					return .Cons((rightDiff, rightCost), right)
				} else {
					return .Cons((downDiff, downCost), down)
				}
			}
		}

		// right extent of the edit graph; can only move down
		if let down = down {
			let diff = Diff.Insert(b[j])
			let cost = cost(diff) + (down.value.first?.1 ?? 0)
			return .Cons((diff, cost), down)
		}

		// bottom extent of the edit graph; can only move right
		if let right = right {
			let diff = Diff.Delete(a[i])
			let cost = cost(diff) + (right.value.first?.1 ?? 0)
			return .Cons((diff, cost), right)
		}

		// bottom-right corner of the edit graph
		return Stream.Nil
	}

	return Array(matrix[a.startIndex, b.startIndex]!.value.map { diff, _ in diff })
}


import Memo
import Prelude
import Stream
