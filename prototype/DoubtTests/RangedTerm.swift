struct RangedTerm {
	let term: Cofree<String, Range<String.Index>>
}


extension RangedTerm: Arbitrary {
	static var arbitrary: Gen<RangedTerm> {
		return Gen.oneOf([])
	}
}


@testable import Doubt
import SwiftCheck
