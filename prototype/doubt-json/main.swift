import Doubt
import Cocoa

enum JSONLeaf {
	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Null
}

extension JSON {
	init?(path: Swift.String) {
		guard let data = try? NSData(contentsOfFile: path, options: []) else { return nil }
		guard let object = try? NSJSONSerialization.JSONObjectWithData(data, options: .AllowFragments) else { return nil }
		self.init(object: object)
	}

	typealias Term = Fix<JSONLeaf>
}
