final class SwiftTests: XCTestCase {
	func testParsingAFile() {
		let path = __FILE__
		guard let file = File(path: path) else {
			XCTFail("Could not make a File from \(__FILE__)")
			return
		}

		let structure = Structure(file: file)
		let dictionary = toAnyObject(structure.dictionary)

		print(JSON(object: dictionary).map { Term(path: path, JSON: $0) })
	}
}


@testable import Doubt
import SourceKittenFramework
import XCTest
