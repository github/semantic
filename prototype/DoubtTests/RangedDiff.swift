struct RangedDiff {
	typealias Term = RangedTerm.Term
	typealias Diff = Free<String, Patch<Term>>

	let a: Term
	let b: Term
	let diff: Diff
}

extension RangedDiff: Arbitrary {
	static let interpreter = Interpreter<Term>(equal: ==, comparable: const(true), cost: Diff.sum(Patch.difference))

	static var arbitrary: Gen<RangedDiff> {
		return RangedTerm.arbitrary.bind { a in
			RangedTerm.arbitrary.fmap { b in
				RangedDiff(a: a.term, b: b.term, diff: interpreter.run(a.term, b.term))
			}
		}
	}
}


import Doubt
import Prelude
import SwiftCheck
