struct RangedDiff {
	typealias Term = RangedTerm.Term
	typealias Diff = Free<String, Patch<Term>>

	let diff: Diff
}


import Doubt
