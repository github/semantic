/// Computes the SES (shortest edit script), i.e. the shortest sequence of diffs (`Free<Leaf, Annotation, Patch<Term>>`) for two arrays of `Term`s which would suffice to transform `a` into `b`.
///
/// This is computed w.r.t. an `equals` function, which computes the equality of leaf nodes within terms, and a `recur` function, which produces diffs representing matched-up terms.
public func SES<Term, Leaf, Annotation>(a: [Term], _ b: [Term], cost: Free<Leaf, Annotation, Patch<Term>> -> Int, recur: (Term, Term) -> Free<Leaf, Annotation, Patch<Term>>?) -> [Free<Leaf, Annotation, Patch<Term>>] {
	typealias Diff = Free<Leaf, Annotation, Patch<Term>>

	if a.isEmpty { return b.map { .Insert($0) } }
	if b.isEmpty { return a.map { .Delete($0) } }

	func cons(diff: Diff, rest: Memo<Stream<(Diff, Int)>>) -> Stream<(Diff, Int)> {
		return .Cons((diff, cost(diff) + costOfStream(rest)), rest)
	}

	func costOfStream(stream: Memo<Stream<(Diff, Int)>>) -> Int {
		return stream.value.first?.1 ?? 0
	}

	func min<A>(a: A, _ rest: A..., _ isLessThan: (A, A) -> Bool) -> A {
		return rest.reduce(a, combine: { isLessThan($0, $1) ? $0 : $1 })
	}

	// A matrix whose values are streams representing paths through the edit graph, carrying both the diff & the cost of the remainder of the path.
	var matrix: Matrix<Stream<(Diff, Int)>, Int>!
	matrix = Matrix(across: a.indices, down: b.indices) { i, j in
		// Some explanation is warranted:
		//
		// 1. `matrix` captures itself during construction, because each vertex in the edit graph depends on other vertices. This is safe, because a) `Matrix` populates its fields lazily, and b) vertices only depend on those vertices downwards and rightwards of them.
		//
		// 2. `matrix` is sized bigger than `a.count` x `b.count`. This is safe, because a) we only get a[i]/b[j] when right/down are non-nil (respectively), and b) right/down are found by looking up elements (i + 1, j) & (i, j + 1) in the matrix, which returns `nil` when out of bounds. So we only access a[i] and b[j] when i and j are in bounds.

		let right = matrix[i + 1, j]
		let down = matrix[i, j + 1]
		let diagonal = matrix[i + 1, j + 1]

		if let right = right, down = down, diagonal = diagonal {
			let here = recur(a[i], b[j])
			// If the diff at this vertex is zero-cost, we’re not going to find a cheaper one either rightwards or downwards. We can therefore short-circuit selecting the best outgoing edge and save ourselves evaluating the entire row rightwards and the entire column downwards from this point.
			//
			// Thus, in the best case (two equal sequences), we now complete in O(n + m). However, this optimization only applies to equalities at the beginning of the edit graph; once inequalities are encountered, the remainder of the diff is effectively O(nm).
			if let here = here where cost(here) == 0 { return cons(here, rest: diagonal) }
			let right = (right, Diff.Delete(a[i]), costOfStream(right))
			let down = (down, Diff.Insert(b[j]), costOfStream(down))
			let diagonal = here.map { (diagonal, $0, costOfStream(diagonal)) }
			// nominate the best edge to continue along, not considering diagonal if `recur` returned `nil`.
			let (best, diff, _) = diagonal
				.map { min($0, right, down) { $0.2 < $1.2 } }
				?? min(right, down) { $0.2 < $1.2 }
			return cons(diff, rest: best)
		}

		// right extent of the edit graph; can only move down
		if let down = down {
			return cons(Diff.Insert(b[j]), rest: down)
		}

		// bottom extent of the edit graph; can only move right
		if let right = right {
			return cons(Diff.Delete(a[i]), rest: right)
		}

		// bottom-right corner of the edit graph
		return Stream.Nil
	}

	return Array(matrix[0, 0]!.value.map { diff, _ in diff })
}


import Memo
import Prelude
import Stream
