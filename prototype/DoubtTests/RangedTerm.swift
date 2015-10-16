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
		let indexed: Gen<Term> = Gen.sized { n in
			Gen<Int>.choose((0, n)).bind { n in
				sequence((0..<n).map(const(UnannotatedTerm.arbitrary))).fmap {
					Term((), .Indexed($0.map { $0.term }))
				}
			}
		}
		return Gen.oneOf([
			leaf,
			indexed,
		]).fmap {
			UnannotatedTerm(term: $0)
		}
	}
}


@testable import Doubt
import Prelude
import SwiftCheck
