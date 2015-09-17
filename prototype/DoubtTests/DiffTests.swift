final class DiffTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		XCTAssertEqual(Diff.diff([], []), [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		XCTAssertEqual(Diff.diff([], [ a, b ]), [ Diff.Patch(.Empty, a), Diff.Patch(.Empty, b) ])
	}

	func testSESOverNonEmptyAndEmptyCollectionsIsDeletions() {
		XCTAssertEqual(Diff.diff([ a, b ], []), [ Diff.Patch(a, .Empty), Diff.Patch(b, .Empty) ])
	}

	func testSESCanInsertAtHead() {
		XCTAssertEqual(Diff.diff([ a, b, c ], [ d, a, b, c ]), [ Diff.Insert(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteAtHead() {
		XCTAssertEqual(Diff.diff([ d, a, b, c ], [ a, b, c ]), [ Diff.Delete(d), Diff(a), Diff(b), Diff(c) ])
	}
}

private let a = Fix(.Literal("a"))
private let b = Fix(.Literal("b"))
private let c = Fix(.Literal("c"))
private let d = Fix(.Literal("d"))

import Doubt
import XCTest
