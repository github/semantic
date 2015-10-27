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


let document = ts_document_make()
ts_document_set_language(document, ts_language_javascript())
