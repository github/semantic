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

	func testParsingAFile() {
		guard let file = File(path: __FILE__) else {
			XCTFail("Could not make a File from \(__FILE__)")
			return
		}

		let structure = Structure(file: file)
		print(toAnyObject(structure.dictionary))

		let prism: Prism<AnyObject, [(String, [String])]> = JSON.JSON.dictionary["key.substructure"].array.map {
			$0.dictionary["key.name"].string &&& $0.dictionary["key.substructure"].array.map { $0.dictionary["key.name"].string }
		}
		let focus = prism.forward(toAnyObject(structure.dictionary))
		XCTAssertEqual(focus?[0].0, "SwiftTests")
		XCTAssertEqual(focus?[0].1 ?? [], ["testValuesCanBeAlphabetic()", "testKeyValueCanBeQuoted()", "testQuotedMatchesQuotedStrings()", "testBranchesStartWithAnIdentifier()", "testBranchesDoNotRequireChildren()", "testParsingAFile()"])
	}
}


@testable import Doubt
import SourceKittenFramework
import XCTest
