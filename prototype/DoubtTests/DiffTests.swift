final class DiffTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		XCTAssertEqual(Diff.diff([], []), [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		XCTAssertEqual(Diff.diff([], [ a, b ]), [ Diff.Patch(nil, a), Diff.Patch(nil, b) ])
	}

	func testSESOverNonEmptyAndEmptyCollectionsIsDeletions() {
		XCTAssertEqual(Diff.diff([ a, b ], []), [ Diff.Patch(a, nil), Diff.Patch(b, nil) ])
	}

	func testSESCanInsertAtHead() {
		XCTAssertEqual(Diff.diff([ a, b, c ], [ d, a, b, c ]), [ Diff.Insert(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteAtHead() {
		XCTAssertEqual(Diff.diff([ d, a, b, c ], [ a, b, c ]), [ Diff.Delete(d), Diff(a), Diff(b), Diff(c) ])
	}

	func testSESCanInsertInMiddle() {
		XCTAssertEqual(Diff.diff([ a, b, c ], [ a, d, b, c ]), [ Diff(a), Diff.Insert(d), Diff(b), Diff(c) ])
	}

	func testSESCanDeleteInMiddle() {
		XCTAssertEqual(Diff.diff([ a, d, b, c ], [ a, b, c ]), [ Diff(a), Diff.Delete(d), Diff(b), Diff(c) ])
	}

	func testSESOfLongerSequences() {
		// fixme: this is awfully slow for such a short sequence
		XCTAssertEqual(Diff.diff([ a, b, c, a, b, b, a ], [ c, b, a, b, a, c ]), [ Diff.Patch(a, c), Diff(b), Diff.Delete(c), Diff(a), Diff.Delete(b), Diff(b), Diff(a), Diff.Insert(c) ])
	}
}

private typealias Term = Fix<Info>

private let a = Term.In(.Leaf(.Literal("a", [])))
private let b = Term.In(.Leaf(.Literal("b", [])))
private let c = Term.In(.Leaf(.Literal("c", [])))
private let d = Term.In(.Leaf(.Literal("d", [])))


@testable import Doubt
import XCTest
