final class DiffTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		assert(SES([], []), ==, [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		assert(SES([], [ a, b ]), ==, [ insert(a), insert(b) ])
	}

	func testSESOverNonEmptyAndEmptyCollectionsIsDeletions() {
		assert(SES([ a, b ], []), ==, [ Diff.Pure(.Delete(a)), Diff.Pure(.Delete(b)) ])
	}

	func testSESCanInsertAtHead() {
		assert(SES([ a, b, c ], [ d, a, b, c ]), ==, [ insert(d), roll(a), roll(b), roll(c) ])
	}

	func testSESCanDeleteAtHead() {
		assert(SES([ d, a, b, c ], [ a, b, c ]), ==, [ delete(d), roll(a), roll(b), roll(c) ])
	}

	func testSESCanInsertInMiddle() {
		assert(SES([ a, b, c ], [ a, d, b, c ]), ==, [ roll(a), insert(d), roll(b), roll(c) ])
	}

	func testSESCanDeleteInMiddle() {
		assert(SES([ a, d, b, c ], [ a, b, c ]), ==, [ roll(a), delete(d), roll(b), roll(c) ])
	}

	func testSESOfLongerSequences() {
		// fixme: this is awfully slow for such a short sequence
		XCTAssertEqual(Diff.diff([ a, b, c, a, b, b, a ], [ c, b, a, b, a, c ]), [ Diff.Patch(a, c), Diff(b), Diff.Delete(c), Diff(a), Diff.Delete(b), Diff(b), Diff(a), Diff.Insert(c) ])
	}
}

private func insert(term: Term) -> Diff {
	return Diff.Pure(.Insert(term))
}

private func delete(term: Term) -> Diff {
	return Diff.Pure(.Delete(term))
}

private func roll(term: Term) -> Diff {
	return Diff.Roll(term.out.map(roll))
}

private typealias Term = Fix<Info>
private typealias Diff = Free<Info, Patch<Info>>

private let a = Term.In(.Leaf(.Literal("a", [])))
private let b = Term.In(.Leaf(.Literal("b", [])))
private let c = Term.In(.Leaf(.Literal("c", [])))
private let d = Term.In(.Leaf(.Literal("d", [])))

private func SES(a: [Term], _ b: [Term]) -> [Diff] {
	return SES(a, b, equals: ==, recur: { Diff.Pure(Patch.Replace($0, $1)) })
}


import Assertions
@testable import Doubt
import XCTest
