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

	func testCannotMoveDownwardsFromLeaves() {
		assert(leaf.explore().down?.it, ==, nil)
	}

	func testCanMoveDownwardsIntoBranches() {
		assert(term.explore().down?.it, ==, leaf)
	}

	func testCanMoveBackUpwards() {
		assert(term.explore().down?.up?.it, ==, term)
	}
}


private let leaf = Cofree(1, .Leaf("a string"))
private let innerLeaf = Cofree(4, .Leaf("a nested string"))
private let term: Cofree<String, Int> = Cofree(0, .Indexed([
	leaf,
	Cofree(2, .Leaf("b string")),
	Cofree(3, .Keyed([
		"a": innerLeaf,
		"b": Cofree(5, .Leaf("b nested string")),
	])),
]))


import Assertions
@testable import Doubt
import XCTest
