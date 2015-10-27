import Cocoa
import Darwin
import Doubt


extension TSInput {
	init(path: String) {
		let file = fopen(path, "r")
		self.init(
			payload: file,
			read_fn: { (payload: UnsafeMutablePointer<Void>, bytesRead: UnsafeMutablePointer<Int>) -> UnsafePointer<Int8> in
				let result = UnsafePointer<Int8>(fgets(nil, 100, UnsafeMutablePointer<FILE>(payload)))
				bytesRead.memory = Int(strlen(result))
				return result
			},
			seek_fn: { (payload: UnsafeMutablePointer<Void>, position: TSLength) -> Int32 in
				fseek(UnsafeMutablePointer<FILE>(payload), position.bytes, SEEK_CUR)
			})
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let a = arguments[0].map(TSInput.init) {
	let document = ts_document_make()
	ts_document_set_language(document, ts_language_javascript())
	ts_document_set_input(document, a)
	ts_document_parse(document)
	let root = ts_document_root_node(document)
	print(ts_node_name(root, document))
	ts_document_free(document)
}
