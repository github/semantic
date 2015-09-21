final class SwiftTests: XCTestCase {
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
		XCTAssertEqual(focus?[0].1 ?? [], ["testParsingAFile()"])
	}
}


@testable import Doubt
import SourceKittenFramework
import XCTest
