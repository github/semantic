import Cocoa
import Doubt
import Either
import Prelude
import Madness


let arguments = BoundsCheckedArray(array: Process.arguments)

let empty = "{}\n"
print(parse(json, input: empty))
let dict = "{\"hello\":\"world\"}"
print(parse(json, input: dict))
let dictWithSpaces = "{ \"hello\" : \"world\" }"
print(parse(json, input: dictWithSpaces))

let dictWithMembers = "{\"hello\":\"world\",\"sup\":\"cat\"}"
print(parse(json, input: dictWithMembers))

let dictWithArray = "{\"hello\": [\"world\"],\"sup\": [\"cat\", \"dog\", \"keith\"] }"
print(parse(json, input: dictWithArray))

func diffAndSerialize(a aString: String, b bString: String) -> String? {
	let aParsed = curry(parse)(json)(aString)
	guard let a = aParsed.right else {
		_ = aParsed.left.map { print("error parsing a:", $0) }
		return nil
	}
	let bParsed = curry(parse)(json)(bString)
	guard let b = bParsed.right else {
		_ = bParsed.left.map { print("error parsing b:", $0) }
		return nil
	}

	let diff = Interpreter<CofreeJSON>(equal: CofreeJSON.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)

	guard let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON(a: aString, b: bString) }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) else { return nil }

	return JSON as String
}

let readFile = { (path: String) -> String? in
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

if let a = arguments[1].flatMap(readFile), b = arguments[2].flatMap(readFile), diff = diffAndSerialize(a: a, b: b) {
	print(diff)
}
