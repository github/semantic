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
		assert(leafA.explore().down?.it, ==, nil)
	}

	func testCanMoveDownwardsIntoBranches() {
		assert(term.explore().down?.it, ==, leafA)
	}

	func testCanMoveBackUpwards() {
		assert(term.explore().down?.up?.it, ==, term)
	}

	func testCannotMoveLeftwardsFromFirstChildOfBranch() {
		assert(term.explore().down?.left?.it, ==, nil)
	}

	func testCanMoveRightwardsFromLeftmostChildOfLongBranch() {
		assert(term.explore().down?.right?.it, ==, leafB)
	}

	func testCanExploreBranchesDeeply() {
		assert(term.explore().down?.right?.right?.down?.it, ==, innerLeafB)
	}

	func testCanMoveBackUpwardsFromDeepExplorations() {
		assert(term.explore().down?.right?.right?.down?.right?.up?.up?.it, ==, term)
	}

	func testCanReturnToStartOfExplorationFromArbitrarilyDeepNodes() {
		assert(term.explore().down?.right?.right?.down?.right?.root.it, ==, term)
	}

	func testSequenceIsPreOrderDepthFirstTraversal() {
		assert(term.explore().map { $0.it.extract }, ==, [ 0, 1, 2, 3, 5, 4 ])
	}

	func testModifyReplacesSubtrees() {
		assert(term.explore().down?.modify(const(leafB)).right?.up?.it, ==, Cofree(0, .Indexed([ leafB, leafB, keyed ])))
	}

	func testMultipleModificationsReplaceMultipleSubtrees() {
		assert(term.explore().down?.modify(const(leafB)).right?.modify(const(leafA)).up?.it, ==, Cofree(0, .Indexed([ leafB, leafA, keyed ])))
	}
}


private let leafA = Cofree(1, .Leaf("a string"))
private let leafB = Cofree(2, .Leaf("b string"))
private let innerLeafA = Cofree(4, .Leaf("a nested string"))
private let innerLeafB = Cofree(5, .Leaf("b nested string"))
private let keyed = Cofree(3, .Keyed([
	"a": innerLeafA,
	"b": innerLeafB,
]))
private let term: Cofree<String, Int> = Cofree(0, .Indexed([
	leafA,
	leafB,
	keyed,
]))


import Assertions
@testable import Doubt
import Prelude
import XCTest
