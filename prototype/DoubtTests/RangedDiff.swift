struct RangedDiff {
	typealias Diff = Free<String, Patch<RangedTerm.Term>>

	let a: RangedTerm
	let b: RangedTerm
	let diff: Diff
}

extension RangedDiff: Arbitrary {
	static let interpreter = Interpreter<RangedTerm.Term>(equal: ==, comparable: const(true), cost: Diff.sum(Patch.difference))

	static var arbitrary: Gen<RangedDiff> {
		return RangedTerm.arbitrary.bind { a in
			RangedTerm.arbitrary.fmap { b in
				RangedDiff(a: a, b: b, diff: interpreter.run(a.term, b.term))
			}
		}
	}
}


import Doubt
import Prelude
import SwiftCheck
