final class TermTests: XCTestCase {
	override static func setUp() {
		sranddev()
	}

	func testEqualityIsReflexive() {
		property("equality is reflexive") <- forAll { (term: RangedTerm) in
			Cofree.equals(annotation: ==, leaf: ==)(term.term, term.term)
		}
	}
}


@testable import Doubt
import SwiftCheck
import XCTest
