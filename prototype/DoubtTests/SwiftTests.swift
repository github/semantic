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
			switch d["key.kind"]?.string {
			case .Some("source.lang.swift.decl.class"), .Some("source.lang.swift.decl.extension"):
				self = .Group(.Literal(name), substructure.map(Term.init))

			case .Some("source.lang.swift.decl.function.method.instance"), .Some("source.lang.swift.decl.function.free"):
				self = .Assign(name, .Abstract([], substructure.map(Term.init)))

			default:
				self = .Empty
			}
		default:
			self = .Empty
		}
	}
}


@testable import Doubt
import SourceKittenFramework
import XCTest
