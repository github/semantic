final class SwiftTests: XCTestCase {
	func testValuesCanBeAlphabetic() {
		XCTAssertEqual(full(SwiftAST.Parsers.keyValue)("key=value"), .KeyValue("key", "value"))
	}

	func testKeyValueCanBeQuoted() {
		XCTAssertEqual(full(SwiftAST.Parsers.keyValue)("key='value'"), .KeyValue("key", "'value'"))
	}

	func testQuotedMatchesQuotedStrings() {
		XCTAssertEqual(full(SwiftAST.Parsers.quoted)("'value'"), "'value'")
	}

	func testBranchesStartWithAnIdentifier() {
		XCTAssertEqual(full(SwiftAST.Parsers.branch)("(a b=c)"), .Branch("a", [ .KeyValue("b", "c") ]))
	}

	func testBranchesDoNotRequireChildren() {
		XCTAssertEqual(full(SwiftAST.Parsers.branch)("(return_stmt)"), .Branch("return_stmt", []))
	}
}


@testable import Doubt
import XCTest
