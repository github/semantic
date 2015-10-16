final class DiffTests: XCTestCase {
	override static func setUp() {
		sranddev()
	}

	typealias Term = RangedTerm.Term
	typealias Diff = Free<String, Patch<Term>>
}


@testable import Doubt
import SwiftCheck
import XCTest
