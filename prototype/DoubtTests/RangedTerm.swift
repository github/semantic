struct RangedTerm {
	typealias Term = Cofree<String, (String, Range<Int>)>
	let term: Term
}

struct UnannotatedTerm {
	typealias Term = Cofree<String, ()>
	let term: Term
}


extension UnannotatedTerm: Arbitrary {
	static var arbitrary: Gen<UnannotatedTerm> {
		let leaf: Gen<Term> = String.arbitrary.fmap { Term((), .Leaf($0)) }
		return Gen.oneOf([
			leaf,
		]).fmap {
			UnannotatedTerm(term: $0)
		}
	}
}


@testable import Doubt
import SwiftCheck
