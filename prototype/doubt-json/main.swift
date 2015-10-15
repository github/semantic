import Cocoa
import Doubt
import Either
import Prelude
import Madness

typealias Term = Cofree<JSONLeaf, Int>
typealias Diff = Free<JSONLeaf, Patch<Term>>

enum JSONLeaf: CustomJSONConvertible, CustomStringConvertible, Equatable {
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
			return Cofree(size(json), json)
		}
		func size(syntax: Syntax<Term, JSONLeaf>) -> Int {
			switch syntax {
			case .Leaf:
				return 1
			case let .Indexed(i):
				return 1 + i.map { size($0.unwrap) }.reduce(0, combine: +)
			case let .Keyed(i):
				return 1 + i.values.map { size($0.unwrap) }.reduce(0, combine: +)
			}
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

let file = NSData(contentsOfFile: Process.arguments[1])

typealias CofreeJSON = Cofree<JSONLeaf, Range<String.CharacterView.Index>>
typealias JSONParser = Parser<String, CofreeJSON>.Function

extension String: CollectionType {
	public var count: Index.Distance {
		return characters.count
	}
}

func not<C: CollectionType, T>(parser: Parser<C, T>.Function)(_ input: C, _ index: C.Index) -> Either<Error<C.Index>, (C.Generator.Element, C.Index)> {
	if index.distanceTo(input.endIndex) <= 0 || parser(input, index).right != nil {
		return .Left(Error(reason: "", index: index, children: []))
	} else {
		return .Right(input[index], index.successor())
	}
}

typealias StringParser = Parser<String, String>.Function

let json: JSONParser = fix { json in
	// TODO: Parse backslashed escape characters
	let stringBody: StringParser = { $0.map({ String($0) }).joinWithSeparator("") } <^>
		not(%"\\" | %"\"")*
	let quoted = { $1.0 } <^> %"\"" ++ stringBody ++ %"\""
	let string: JSONParser = quoted --> { Cofree($1, .Leaf(.String($2))) }
	let array =  %"[" ++ json* ++ %"]" --> { Cofree($1, .Indexed($2.1.0)) }

	let dict: JSONParser =  %"{" ++ (quoted ++ %":" ++ json)* ++ %"}" --> { (_, range: Range<String.Index>, value: (String, ([(String, (String, CofreeJSON))], String))) in
		Cofree(range, .Keyed([value.0: value.1.0[0].1.1]))
	}

	// TODO: Parse Numbers correctly
	let number: JSONParser = %"0" --> { Cofree($1, .Leaf(.String($2))) }

	return dict | array
}


if let a = arguments[1].flatMap(JSON.init), b = arguments[2].flatMap(JSON.init) {
	let diff = Algorithm<Term, Diff>(a.term, b.term).evaluate(Cofree.equals(annotation: const(true), leaf: ==))
	if let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON { $0.JSON } }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) {
		print(JSON)
	}
}
