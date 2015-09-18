final class DoubtTests: XCTestCase {
	func testEqualSyntaxResultsInRecursivelyCopyingDiff() {
		if let s = sexpr("\t(\n( a) \n)\t")?.value, t = sexpr("((a))")?.value {
			XCTAssertEqual(Diff(s, t), Diff.Copy(.Apply(.Copy(.Apply(.Copy(.Variable("a")), [])), [])))
		}
	}
}


@testable import Doubt
import XCTest
