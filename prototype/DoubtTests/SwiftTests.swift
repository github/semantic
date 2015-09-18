final class SwiftTests: XCTestCase {
	func testKeyValueAcceptsAlphabeticKeysAndValues() {
		XCTAssertEqual(Swift.Parsers.keyValue("key=value")?.value, .KeyValue("key", "value"))
	}
}


@testable import Doubt
import XCTest
