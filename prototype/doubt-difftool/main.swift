import Cocoa
import Doubt
import Prelude

func readFile(path: String) -> String? {
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

extension String: ErrorType {}

typealias Term = Cofree<String, Info>

struct Info: Categorizable, CustomJSONConvertible, Equatable {
	let range: Range<Int>


	// MARK: Categorizable

	let categories: Set<String>


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		return [
			"range": range.JSON,
			"categories": Array(categories).JSON
		]
	}
}

func == (left: Info, right: Info) -> Bool {
	return left.range == right.range && left.categories == right.categories
}


extension String.UTF16View {
	subscript (range: Range<Int>) -> String.UTF16View {
		return self[Index(_offset: range.startIndex)..<Index(_offset: range.endIndex)]
	}
}


func termWithInput(string: String) -> Term? {
	let document = ts_document_make()
	defer { ts_document_free(document) }
	return string.withCString {
		ts_document_set_language(document, ts_language_javascript())
		ts_document_set_input_string(document, $0)
		ts_document_parse(document)
		let root = ts_document_root_node(document)

		return try? Cofree
			.ana { node, category in
				let count = node.namedChildren.count
				guard count > 0 else { return Syntax.Leaf(category) }
				switch category {
				case "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary":
					return try .Fixed(node.namedChildren.map {
						($0, try $0.category(document))
					})
				case "object":
					return try .Keyed(Dictionary(elements: node.namedChildren.map {
						switch try $0.category(document) {
						case "pair":
							let range = $0.namedChildren[0].range
							guard let key = String(string.utf16[range]) else { throw "could not make a string from utf16 range '\(range)'" }
							return (key, ($0, "pair"))
						default:
							// We might have a comment inside an object literal. It should still be assigned a key, however.
							return try (String($0.range), ($0, $0.category(document)))
						}
					}))
				default:
					return try .Indexed(node.namedChildren.map {
						($0, try $0.category(document))
					})
				}
			} (root, "program")
			.map { node, category in
				Info(range: node.range, categories: [ category ])
			}
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let aString = arguments[1].flatMap(readFile), bString = arguments[2].flatMap(readFile), c = arguments[3], ui = arguments[4] {
	if let a = termWithInput(aString), b = termWithInput(bString) {
		let diff = Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: Interpreter<Term>.comparable { $0.extract.categories }, cost: Free.sum(Patch.difference)).run(a, b)
		let JSON: Doubt.JSON = [
			"before": .String(aString),
			"after": .String(bString),
			"diff": diff.JSON(pure: { $0.JSON { $0.JSON(annotation: { $0.range.JSON }, leaf: Doubt.JSON.String) } }, leaf: Doubt.JSON.String, annotation: {
				[
					"before": $0.range.JSON,
					"after": $1.range.JSON,
				]
			}),
		]
		let data = JSON.serialize()
		try data.writeToFile(c, options: .DataWritingAtomic)

		let components = NSURLComponents()
		components.scheme = "file"
		components.path = ui
		components.query = c
		if let URL = components.URL {
			NSWorkspace.sharedWorkspace().openURL(URL)
		}
	}
}
