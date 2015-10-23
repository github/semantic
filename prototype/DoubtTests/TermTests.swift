final class TermTests: XCTestCase {
	override static func setUp() {
		sranddev()
	}

	func testEqualityIsReflexive() {
		property("equality is reflexive") <- forAll { (term: RangedTerm) in
			Cofree.equals(annotation: ==, leaf: ==)(term.term, term.term)
		}
	}

	func testEqualTermsZipCleanly() {
		property("equal terms zip to a non-nil value") <- forAll { (term: RangedTerm) in
			Cofree.zip(term.term, term.term) != nil
		}
	}
}


@testable import Doubt
import SwiftCheck
import XCTest
