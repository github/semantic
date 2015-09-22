final class SwiftTests: XCTestCase {
	func testParsingAFile() {
		let path = __FILE__
		guard let file = File(path: path) else {
			XCTFail("Could not make a File from \(__FILE__)")
			return
		}

		let structure = Structure(file: file)
		let dictionary = toAnyObject(structure.dictionary)
		print(dictionary)

		let prism: Prism<AnyObject, [(String, [String])]> = JSON.JSON.dictionary["key.substructure"].array.map {
			$0.dictionary["key.name"].string &&& $0.dictionary["key.substructure"].array.map { $0.dictionary["key.name"].string }
		}

		print(JSON(object: dictionary).map { Term(path: path, JSON: $0) })

		let focus = prism.forward(dictionary)
		XCTAssertEqual(focus?[0].0, "SwiftTests")
		XCTAssertEqual(focus?[0].1 ?? [], ["testParsingAFile()"])
	}
}

extension Term {
	init(path: String, JSON: Doubt.JSON) {
		switch JSON.dictionary?["key.substructure"] {
		case let .Some(.Array(a)):
			self = .Roll(.Group(.Roll(.Literal(path)), a.map(Term.init)))
		default:
			self = .Empty
		}
	}

	init(JSON: Doubt.JSON) {
		switch JSON.dictionary {
		case let .Some(d) where d["key.name"] != nil && d["key.substructure"] != nil:
			let name = d["key.name"]?.string ?? ""
			let substructure = d["key.substructure"]?.array ?? []
			self = .Roll(.Group(.Roll(.Literal(name)), substructure.map(Term.init)))
		default:
			self = .Empty
		}
	}
}


@testable import Doubt
import SourceKittenFramework
import XCTest
