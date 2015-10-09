import Doubt
import Cocoa

enum JSONLeaf: Equatable, CustomJSONConvertible, CustomStringConvertible {
	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Null


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		switch self {
		case let .Number(n):
			return .Number(n)
		case let .Boolean(b):
			return .Boolean(b)
		case let .String(s):
			return .String(s)
		case .Null:
			return .Null
		}
	}


	// MARK: CustomStringConvertible

	var description: Swift.String {
		switch self {
		case let .Number(n):
			return Swift.String(n)
		case let .Boolean(b):
			return Swift.String(b)
		case let .String(s):
			return Swift.String(reflecting: s)
		case .Null:
			return "null"
		}
	}
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
	typealias Diff = Free<JSONLeaf, Patch<JSONLeaf>>

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

let arguments = BoundsCheckedArray(array: Process.arguments)
if let a = arguments[1].flatMap(JSON.init), b = arguments[2].flatMap(JSON.init) {
	let diff = Algorithm<JSONLeaf, JSON.Diff>(a.term, b.term).evaluate()
	if let JSON = NSString(data: diff.JSON.serialize(), encoding: NSUTF8StringEncoding) {
		print(JSON)
	}
}
