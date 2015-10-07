final class DocTests: XCTestCase {
	func testEmptyDocLaysOutToEmptyString() {
		assert(Doc.Empty.description, ==, "")
	}
}


import Assertions
@testable import Doubt
import XCTest
