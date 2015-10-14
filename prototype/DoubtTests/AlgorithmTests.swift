final class AlgorithmTests: XCTestCase {
	func testRestrictsComparisonsWhenRecurReturnsNil() {
		assert(Algorithm(a, b).evaluate(==, recur: const(nil)), ==, restricted)
	}
}


private typealias Term = Cofree<String, Int>
private typealias Diff = Free<String, Patch<Term>>

private let a = Term(0, [ Term(1, .Leaf("a")), Term(2, .Leaf("b")), Term(3, .Leaf("c")) ])
private let b = Term(0, [ Term(1, .Leaf("c")), Term(2, .Leaf("b")), Term(3, .Leaf("a")) ])

private let restricted: Diff = Diff.Roll([
	.Pure(.Insert(Term(1, .Leaf("c")))),
	.Pure(.Delete(Term(1, .Leaf("a")))),
	Diff(Term(2, .Leaf("b"))),
	.Pure(.Insert(Term(3, .Leaf("a")))),
	.Pure(.Delete(Term(3, .Leaf("c")))),
])


import Assertions
@testable import Doubt
import Prelude
import XCTest
