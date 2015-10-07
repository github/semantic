import Doubt
import Cocoa

enum JSONLeaf: Equatable {
	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Null
}

func == (left: JSONLeaf, right: JSONLeaf) -> Bool {
	switch (left, right) {
	case let (.Number(a), .Number(b)):
		return a == b
	case let (.Boolean(a), .Boolean(b)):
		return a == b
	case let (.String(a), .String(b)):
		return a == b
	case (.Null, .Null):
		return true
	default:
		return false
	}
}


extension JSON {
	init?(path: Swift.String) {
		guard let data = try? NSData(contentsOfFile: path, options: []) else { return nil }
		guard let object = try? NSJSONSerialization.JSONObjectWithData(data, options: .AllowFragments) else { return nil }
		self.init(object: object)
	}

	typealias Term = Fix<JSONLeaf>

	var term: Term {
		switch self {
		case let .Array(a):
			return .In(.Indexed(a.map { $0.term }))
		case let .Dictionary(d):
			return .In(.Keyed(Swift.Dictionary(elements: d.map { ($0, $1.term) })))
		case let .Number(n):
			return .In(.Leaf(.Number(n)))
		case let .Boolean(b):
			return .In(.Leaf(.Boolean(b)))
		case let .String(s):
			return .In(.Leaf(.String(s)))
		case .Null:
			return .In(.Leaf(.Null))
		}
	}
}
