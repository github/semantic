final class DiffTests: XCTestCase {
	func testSESOverEmptyCollectionsIsEmpty() {
		XCTAssertEqual(Diff.diff([], []), [])
	}

	func testSESOverEmptyAndNonEmptyCollectionsIsInsertions() {
		XCTAssertEqual(Diff.diff([], [ a, b ]), [ Diff.Patch(.Empty, a), Diff.Patch(.Empty, b) ])
	}
}

private let a = Fix(.Literal("a"))
private let b = Fix(.Literal("b"))

import Doubt
import XCTest
