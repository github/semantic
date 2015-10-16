final class DiffTests: XCTestCase {
	override static func setUp() {
		sranddev()
	}

	typealias Term = RangedTerm.Term
	typealias Diff = Free<String, Patch<Term>>

	let interpreter = Interpreter<Term>(equal: ==, comparable: const(true), cost: Diff.sum(const(1)))
}


@testable import Doubt
import Prelude
import SwiftCheck
import XCTest
