final class LocationTests: XCTestCase {
	func testExplorationOfATermBeginsAtTheExploredTerm() {
		assert(term.explore().it, ==, term)
	}

	func testCannotMoveUpwardsAtTheStartOfAnExploration() {
		assert(term.explore().up?.it, ==, nil)
	}

	func testCannotMoveSidewaysAtTheStartOfAnExploration() {
		assert(term.explore().left?.it, ==, nil)
		assert(term.explore().right?.it, ==, nil)
	}
}


private let leaf = Cofree(1, .Leaf("a string"))
private let term: Cofree<String, Int> = Cofree(0, .Indexed([
	leaf,
	Cofree(2, .Leaf("b string")),
	Cofree(3, .Keyed([
		"a": Cofree(4, .Leaf("a nested string")),
		"b": Cofree(5, .Leaf("b nested string")),
	])),
]))


import Assertions
@testable import Doubt
import XCTest
