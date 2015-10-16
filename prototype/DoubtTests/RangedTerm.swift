struct RangedTerm {
	typealias Term = Cofree<String, (String, Range<Int>)>
	let term: Term
}

struct UnannotatedTerm {
	typealias Term = Cofree<String, ()>
	let term: Term
}

extension RangedTerm: Arbitrary {
	static var arbitrary: Gen<RangedTerm> {
		func arbitrary(from: Int) -> Gen<Term> {
			let leaf: Gen<Term> = String.arbitrary.fmap {
				Cofree(($0, from..<Int($0.characters.count)), .Leaf($0))
			}
			return Gen.oneOf([
				leaf,
			])
		}
		return arbitrary(0).fmap {
			RangedTerm(term: $0)
		}
	}
}


@testable import Doubt
import SwiftCheck
