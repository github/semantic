import Cocoa
import Doubt
import Either
import Prelude
import Madness


let arguments = BoundsCheckedArray(array: Process.arguments)

let empty = "{}"
print(parse(json, input: empty))
let dict = "{\"hello\":\"world\"}"
print(parse(json, input: dict))
let dictWithSpaces = "{ \"hello\" : \"world\" }"
print(parse(json, input: dictWithSpaces))

let dictWithMembers = "{\"hello\":\"world\",\"sup\":\"cat\"}"
print(parse(json, input: dictWithMembers))

let dictWithArray = "{\"hello\": [\"world\"],\"sup\": [\"cat\", \"dog\", \"keith\"] }"
print(parse(json, input: dictWithArray))


func readFile(path: String) -> String? {
	return try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) as String
}

func diffAndSerialize(a aString: String, b bString: String) -> String? {
	guard let a = curry(parse)(json)(aString).right, b = curry(parse)(json)(bString).right else { return nil }

	let diff = Interpreter<CofreeJSON>(equal: CofreeJSON.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)

	guard let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON(a: aString, b: bString) }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) else { return nil }

	return JSON as String
}

_ = diffAndSerialize(a: "{\"hello\":\"world\",\"sup\":\"cat\"}", b: "{\"hello\":\"world\",\"sup\":\"dog\"}").map { print($0) }

if let aString = arguments[1].flatMap(readFile), bString = arguments[2].flatMap(readFile), JSON = diffAndSerialize(a: aString, b: bString) {
	print(JSON)
}
