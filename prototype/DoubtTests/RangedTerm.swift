struct RangedTerm {
	typealias Term = Cofree<String, (String, Range<Int>)>
	let term: Term
}

struct UnannotatedTerm {
	typealias Term = Cofree<String, ()>
	let term: Term

	var source: String {
		return term.cata {
			switch $0 {
			case let .Leaf(s):
				return s
			case let .Indexed(s):
				return "[\n\t" + s.joinWithSeparator(",\n\t") + "\n]"
			case let .Keyed(s):
				return "{\n\t" + s.map { "\"\($0)\": \($1)" }.joinWithSeparator(",\n\t") + "\n}"
			}
		}
	}
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
		let keyed: Gen<Term> = Gen.sized { n in
			Gen<Int>.choose((0, n)).bind { n in
				sequence((0..<n).map { _ in String.arbitrary.bind { key in UnannotatedTerm.arbitrary.fmap { (key, $0.term) } } }).fmap {
					Term((), .Keyed(Dictionary(elements: $0)))
				}
			}
		}
		return Gen.oneOf([
			leaf,
			indexed,
			keyed,
		]).fmap {
			UnannotatedTerm(term: $0)
		}
	}
}


@testable import Doubt
import Prelude
import SwiftCheck
