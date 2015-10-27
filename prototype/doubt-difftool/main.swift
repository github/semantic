import Cocoa
import Darwin
import Doubt

extension TSInput {
	init(path: String) {
		let file = fopen(path, "r")
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

let arguments = BoundsCheckedArray(array: Process.arguments)
if let a = arguments[1] {
	guard NSFileManager.defaultManager().fileExistsAtPath(a) else {
		print("\(a): No such file or directory")
		exit(1)
	}
	let a = TSInput(path: a)
	let document = ts_document_make()
	ts_document_set_language(document, ts_language_javascript())
	ts_document_set_input(document, a)
	ts_document_parse(document)
	let root = ts_document_root_node(document)

	print(Cofree<String, TSNode>.ana { node in
		let count = ts_node_child_count(node)
		guard count > 0 else {
			return String.fromCString(ts_node_name(node, document)).map(Syntax.Leaf)!
		}
		return .Indexed((0..<count).map { ts_node_child(node, $0) })
	} (root))

	ts_document_free(document)
}
