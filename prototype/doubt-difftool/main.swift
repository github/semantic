import Cocoa
import Doubt
import Prelude

func readFile(URL: NSURL) -> String? {
	guard let data = try? NSString(contentsOfURL: URL, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

extension String: ErrorType {}

typealias Term = Cofree<String, Info>


extension String.UTF16View {
	subscript (range: Range<Int>) -> String.UTF16View {
		return self[Index(_offset: range.startIndex)..<Index(_offset: range.endIndex)]
	}
}

let languagesByFileExtension: [String:TSLanguage] = [
	"js": ts_language_javascript(),
	"c": ts_language_c(),
]

let keyedProductions: Set<String> = [ "object" ]
let fixedProductions: Set<String> = [ "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary" ]

/// Allow predicates to occur in pattern matching.
func ~= <A> (left: A -> Bool, right: A) -> Bool {
	return left(right)
}


func termWithInput(language: TSLanguage)(_ string: String) -> Term? {
	let document = ts_document_make()
	defer { ts_document_free(document) }
	return string.withCString {
		ts_document_set_language(document, language)
		ts_document_set_input_string(document, $0)
		ts_document_parse(document)
		let root = ts_document_root_node(document)

		return try? Cofree
			.ana { node, category in
				let count = node.namedChildren.count
				guard count > 0 else { return try Syntax.Leaf(node.substring(string)) }
				switch category {
				case fixedProductions.contains:
					return try .Fixed(node.namedChildren.map {
						($0, try $0.category(document))
					})
				case keyedProductions.contains:
					return try .Keyed(Dictionary(elements: node.namedChildren.map {
						switch try $0.category(document) {
						case "pair":
							return try ($0.namedChildren[0].substring(string), ($0, "pair"))
						default:
							// We might have a comment inside an object literal. It should still be assigned a key, however.
							return try (try node.substring(string), ($0, $0.category(document)))
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
if let aURL = arguments[1].flatMap(NSURL.init), aString = readFile(aURL), bURL = arguments[2].flatMap(NSURL.init), bString = readFile(bURL), c = arguments[3], ui = arguments[4] {
	guard aURL.pathExtension == bURL.pathExtension else { throw "can’t compare files of different types" }
	let parser: String -> Term? = termWithInput(ts_language_javascript())
	if let a = parser(aString), b = parser(bString) {
		let diff = Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: Interpreter<Term>.comparable { $0.extract.categories }, cost: Free.sum(Patch.sum)).run(a, b)
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
