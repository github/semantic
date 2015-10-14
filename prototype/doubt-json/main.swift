import Cocoa
import Doubt
import Prelude

typealias Term = Cofree<JSONLeaf, Int>
typealias Diff = Free<JSONLeaf, Patch<Term>>

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

	var term: Term {
		func annotate(json: Syntax<Term, JSONLeaf>) -> Term {
			return Cofree(0, json)
		}
		switch self {
		case let .Array(a):
			return annotate(.Indexed(a.map { $0.term }))
		case let .Dictionary(d):
			return annotate(.Keyed(Swift.Dictionary(elements: d.map { ($0, $1.term) })))
		case let .Number(n):
			return annotate(.Leaf(.Number(n)))
		case let .Boolean(b):
			return annotate(.Leaf(.Boolean(b)))
		case let .String(s):
			return annotate(.Leaf(.String(s)))
		case .Null:
			return annotate(.Leaf(.Null))
		}
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let a = arguments[1].flatMap(JSON.init), b = arguments[2].flatMap(JSON.init) {
	let diff = Algorithm<Term, Diff>(a.term, b.term).evaluate(Cofree.equals(annotation: const(true), leaf: ==))
	if let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON { $0.JSON } }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) {
		print(JSON)
	}
}
