final class SwiftTests: XCTestCase {
	func testValuesCanBeAlphabetic() {
		XCTAssertEqual(full(Swift.Parsers.keyValue)("key=value"), .KeyValue("key", "value"))
	}

	func testKeyValueCanBeQuoted() {
		XCTAssertEqual(full(Swift.Parsers.keyValue)("key='value'"), .KeyValue("key", "'value'"))
	}

	func testQuotedMatchesQuotedStrings() {
		XCTAssertEqual(full(Swift.Parsers.quoted)("'value'"), "'value'")
	}

	func testBranchesStartWithAnIdentifier() {
		XCTAssertEqual(full(Swift.Parsers.branch)("(a b=c)"), .Branch("a", [ .KeyValue("b", "c") ]))
	}
}


@testable import Doubt
import XCTest
