final class SESTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		assert(SES([], []), ==, [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		assert(SES([], [ a, b ]), ==, [ insert(a), insert(b) ])
	}

	func testSESOverNonEmptyAndEmptyCollectionsIsDeletions() {
		assert(SES([ a, b ], []), ==, [ delete(a), delete(b) ])
	}

	func testSESCanInsertAtHead() {
		assert(SES([ a, b, c ], [ d, a, b, c ]), ==, [ insert(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteAtHead() {
		assert(SES([ d, a, b, c ], [ a, b, c ]), ==, [ delete(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanInsertInMiddle() {
		assert(SES([ a, b, c ], [ a, d, b, c ]), ==, [ Diff(a), insert(d), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteInMiddle() {
		assert(SES([ a, d, b, c ], [ a, b, c ]), ==, [ Diff(a), delete(d), Diff(b), Diff(c) ])
	}

	func testInsertsAtEnd() {
		assert(SES([ a, b, c ], [ a, b, c, d ]), ==, [ Diff(a), Diff(b), Diff(c), insert(d) ])
	}

	func testDeletesAtEnd() {
		assert(SES([ a, b, c, d ], [ a, b, c ]), ==, [ Diff(a), Diff(b), Diff(c), delete(d) ])
	}

	func testSESOfLongerSequences() {
		assert(SES([ a, b, c, a, b, b, a ], [ c, b, a, b, a, c ]), ==, [ insert(c), delete(a), Diff(b), delete(c), Diff(a), delete(b), Diff(b), Diff(a), insert(c) ])
	}
}

private func insert(term: Term) -> Diff {
	return Diff.Pure(.Insert(term))
}

private func delete(term: Term) -> Diff {
	return Diff.Pure(.Delete(term))
}

private typealias Term = Cofree<String, ()>
private typealias Diff = Free<String, Patch<Term>>

private let a = Term((), .Leaf("a"))
private let b = Term((), .Leaf("b"))
private let c = Term((), .Leaf("c"))
private let d = Term((), .Leaf("d"))

private func SES(a: [Term], _ b: [Term]) -> [Diff] {
	return SES(a, b) { Cofree.equals(annotation: const(true), leaf: ==)($0, $1) ? Diff($1) : nil }
}

private func == (a: [Diff], b: [Diff]) -> Bool {
	return a.count == b.count && zip(a, b).lazy.map(Diff.equals(ifPure: Patch.equals(Cofree.equals(annotation: const(true), leaf: ==)), ifRoll: ==)).reduce(true) { $0 && $1 }
}


import Assertions
@testable import Doubt
import Prelude
import XCTest
