struct RangedTerm {
	typealias Term = Cofree<String, Range<String.Index>>
	let term: Term
	let source: String

	var stripped: UnannotatedTerm {
		return UnannotatedTerm(term: term.map(const(())))
	}
}

extension RangedTerm: Arbitrary {
	static var arbitrary: Gen<RangedTerm> {
		return UnannotatedTerm.arbitrary.fmap { $0.arranged }
	}

	static func shrink(term: RangedTerm) -> [RangedTerm] {
		return UnannotatedTerm.shrink(term.stripped).map { $0.arranged }
	}
}


@testable import Doubt
import Prelude
import SwiftCheck
