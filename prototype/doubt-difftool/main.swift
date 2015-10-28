import Cocoa
import Doubt
import Prelude

func readFile(path: String) -> String? {
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

typealias Term = Cofree<String, Range<Int>>

func termWithInput(string: String) -> Term? {
	let document = ts_document_make()
	defer { ts_document_free(document) }
	return string.withCString {
		ts_document_set_language(document, ts_language_javascript())
		ts_document_set_input_string(document, $0)
		ts_document_parse(document)
		let root = ts_document_root_node(document)

		return Cofree
			.ana { node in
				let count = ts_node_named_child_count(node)
				let name = String.fromCString(ts_node_name(node, document))
				guard count > 0 else { return name.map(Syntax.Leaf)! }
				return .Indexed((0..<count).map { ts_node_named_child(node, $0) })
			} (root)
			.map {
				let start = ts_node_pos($0).chars
				return start..<(start + ts_node_size($0).chars)
			}
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let aString = arguments[1].flatMap(readFile), bString = arguments[2].flatMap(readFile), c = arguments[3], ui = arguments[4] {
	if let a = termWithInput(aString), b = termWithInput(bString) {
		let diff = Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)
		let range: Range<Int> -> Doubt.JSON = {
			let start = $0.startIndex
			let end = $0.endIndex
			return [
				.Number(Double(start)),
				.Number(Double(end - start)),
			]
		}
		let JSON: Doubt.JSON = [
			"before": .String(aString),
			"after": .String(bString),
			"diff": diff.JSON(pure: { $0.JSON { $0.JSON(annotation: range, leaf: Doubt.JSON.String) } }, leaf: Doubt.JSON.String, annotation: {
				[
					"before": range($0),
					"after": range($1),
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
