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
		assert(SES([ a, b, c ], [ d, a, b, c ]), ==, [ .Insert(d), Copy(a), Copy(b), Copy(c) ])
	}

	func testSESCanDeleteAtHead() {
		assert(SES([ d, a, b, c ], [ a, b, c ]), ==, [ .Delete(d), Copy(a), Copy(b), Copy(c) ])
	}

	func testSESCanInsertInMiddle() {
		assert(SES([ a, b, c ], [ a, d, b, c ]), ==, [ Copy(a), .Insert(d), Copy(b), Copy(c) ])
	}

	func testSESCanDeleteInMiddle() {
		assert(SES([ a, d, b, c ], [ a, b, c ]), ==, [ Copy(a), .Delete(d), Copy(b), Copy(c) ])
	}

	func testInsertsAtEnd() {
		assert(SES([ a, b, c ], [ a, b, c, d ]), ==, [ Copy(a), Copy(b), Copy(c), .Insert(d) ])
	}

	func testDeletesAtEnd() {
		assert(SES([ a, b, c, d ], [ a, b, c ]), ==, [ Copy(a), Copy(b), Copy(c), .Delete(d) ])
	}

	func testSESOfLongerSequences() {
		assert(SES([ a, b, c, a, b, b, a ], [ c, b, a, b, a, c ]), ==, [ .Insert(c), .Delete(a), Copy(b), .Delete(c), Copy(a), .Delete(b), Copy(b), Copy(a), .Insert(c) ])
	}
}

private typealias Term = Cofree<String, ()>
private typealias Diff = Free<String, (), Patch<Term>>

private func Copy(term: Term) -> Diff {
	return hylo(Diff.Introduce(()), Term.unwrap)(term)
}

private let a = Term((), .Leaf("a"))
private let b = Term((), .Leaf("b"))
private let c = Term((), .Leaf("c"))
private let d = Term((), .Leaf("d"))

private func SES(a: [Term], _ b: [Term]) -> [Diff] {
	return SES(a, b, cost: const(1)) { Cofree.equals(annotation: const(true), leaf: ==)($0, $1) ? Copy($1) : nil }
}

private func == (a: [Diff], b: [Diff]) -> Bool {
	return a.count == b.count && zip(a, b).lazy.map(Diff.equals(pure: Patch.equals(Cofree.equals(annotation: const(true), leaf: ==)), leaf: ==, annotation: const(true))).reduce(true) { $0 && $1 }
}


import Assertions
@testable import Doubt
import Prelude
import XCTest
