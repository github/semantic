import Cocoa
import Darwin
import Doubt

extension TSInput {
	init?(path: String) {
		let file = fopen(path, "r")
		guard file != nil else { return nil }
		self.init(
			payload: file,
			read_fn: { (payload: UnsafeMutablePointer<Void>, bytesRead: UnsafeMutablePointer<Int>) -> UnsafePointer<Int8> in
				errno = 0
				var string: UnsafeMutablePointer<Int8> = nil
				var capacity = 0
				var length = getline(&string, &capacity, UnsafeMutablePointer<FILE>(payload))
				if length < 0 {
					if errno == 0 { length = 0 }
					else {
						return nil
					}
				}
				bytesRead.memory = length
				return UnsafePointer<Int8>(string)
			},
			seek_fn: { (payload: UnsafeMutablePointer<Void>, position: TSLength) -> Int32 in
				fseek(UnsafeMutablePointer<FILE>(payload), position.bytes, SEEK_CUR) == 0 ? 1 : 0
			})
	}
}

func readFile(path: String) -> String? {
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

func termWithInput(input: TSInput) -> Cofree<String, Range<Int>>? {
	let document = ts_document_make()
	defer { ts_document_free(document) }
	ts_document_set_language(document, ts_language_javascript())
	ts_document_set_input(document, input)
	ts_document_parse(document)
	let root = ts_document_root_node(document)

	return Cofree
		.ana { node in
			let count = ts_node_named_child_count(node)
			guard count > 0 else {
				return String.fromCString(ts_node_name(node, document)).map(Syntax.Leaf)!
			}
			return .Indexed((0..<count).map { ts_node_named_child(node, $0) })
		} (root)
		.map {
			let start = ts_node_pos($0).chars
			return start..<(start + ts_node_size($0).chars)
		}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let a = arguments[1].flatMap(TSInput.init).flatMap(termWithInput) {
	print(a)
}
