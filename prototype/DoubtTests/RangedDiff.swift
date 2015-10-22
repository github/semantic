struct RangedDiff {
	typealias Diff = Free<String, RangedTerm.Term.Annotation, Patch<RangedTerm.Term>>

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

	static func shrink(diff: RangedDiff) -> [RangedDiff] {
		return RangedTerm.shrink(diff.a).map { RangedDiff(a: $0, b: diff.b, diff: interpreter.run($0.term, diff.b.term)) }
			+ RangedTerm.shrink(diff.b).map { RangedDiff(a: diff.a, b: $0, diff: interpreter.run(diff.a.term, $0.term)) }
	}
}


import Doubt
import Prelude
import SwiftCheck
