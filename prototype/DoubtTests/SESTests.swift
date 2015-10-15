final class SESTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		assert(SES([], []), ==, [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		assert(SES([], [ a, b ]), ==, [ .Insert(a), .Insert(b) ])
	}

	func testSESOverNonEmptyAndEmptyCollectionsIsDeletions() {
		assert(SES([ a, b ], []), ==, [ .Delete(a), .Delete(b) ])
	}

	func testSESCanInsertAtHead() {
		assert(SES([ a, b, c ], [ d, a, b, c ]), ==, [ .Insert(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteAtHead() {
		assert(SES([ d, a, b, c ], [ a, b, c ]), ==, [ .Delete(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanInsertInMiddle() {
		assert(SES([ a, b, c ], [ a, d, b, c ]), ==, [ Diff(a), .Insert(d), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteInMiddle() {
		assert(SES([ a, d, b, c ], [ a, b, c ]), ==, [ Diff(a), .Delete(d), Diff(b), Diff(c) ])
	}

	func testInsertsAtEnd() {
		assert(SES([ a, b, c ], [ a, b, c, d ]), ==, [ Diff(a), Diff(b), Diff(c), .Insert(d) ])
	}

	func testDeletesAtEnd() {
		assert(SES([ a, b, c, d ], [ a, b, c ]), ==, [ Diff(a), Diff(b), Diff(c), .Delete(d) ])
	}

	func testSESOfLongerSequences() {
		assert(SES([ a, b, c, a, b, b, a ], [ c, b, a, b, a, c ]), ==, [ .Insert(c), .Delete(a), Diff(b), .Delete(c), Diff(a), .Delete(b), Diff(b), Diff(a), .Insert(c) ])
	}
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
