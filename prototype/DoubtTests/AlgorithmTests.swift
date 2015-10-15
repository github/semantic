final class AlgorithmTests: XCTestCase {
	func testRestrictsComparisonsWhenRecurReturnsNil() {
		assert(Algorithm(a, b).evaluate(==, recur: const(nil)), ==, restricted)
	}

	func testComparisonsOfDisjointlyCategorizedTermsAreRestricted() {
		var effects = 0
		assert(Algorithm(a, b).evaluate(==, categorize: { _ in [ effects++ ] }), ==, restricted)
	}

	func testComparisonsAreUnrestrictedByDefault() {
		assert(Algorithm(a, b).evaluate(==), ==, unrestricted)
	}

	func testComparisonsOfUncategorizedTermsAreUnrestricted() {
		assert(Algorithm(a, b).evaluate(==, categorize: const(Set<String>())), ==, unrestricted)
	}
}


private typealias Term = Cofree<String, Int>
private typealias Diff = Free<String, Patch<Term>>

private let a = Term(0, [ Term(1, .Leaf("a")), Term(2, .Leaf("b")), Term(3, .Leaf("c")) ])
private let b = Term(0, [ Term(1, .Leaf("c")), Term(2, .Leaf("b")), Term(3, .Leaf("a")) ])

private let restricted = Diff.Roll([
	.Pure(.Insert(Term(1, .Leaf("c")))),
	.Pure(.Delete(Term(1, .Leaf("a")))),
	Diff(Term(2, .Leaf("b"))),
	.Pure(.Insert(Term(3, .Leaf("a")))),
	.Pure(.Delete(Term(3, .Leaf("c")))),
])

private let unrestricted = Diff.Roll([
	.Pure(.Replace(Term(1, .Leaf("a")), Term(1, .Leaf("c")))),
	Diff(Term(2, .Leaf("b"))),
	.Pure(.Replace(Term(3, .Leaf("c")), Term(3, .Leaf("a")))),
])


import Assertions
@testable import Doubt
import Prelude
import XCTest
