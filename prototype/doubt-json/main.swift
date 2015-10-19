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


func data(path: String) -> NSData? {
	return try? NSData(contentsOfFile: path, options: [])
}

func string(data: NSData) -> String? {
	return NSString(data: data, encoding: NSUTF8StringEncoding) as String?
}

func parseJSON(string: String) -> CofreeJSON? {
	return parse(json, input: string).right
}

func diffAndSerialize(a aString: String, b bString: String) -> String? {
	guard let a = parseJSON(aString), b = parseJSON(bString) else { return nil }

	let diff = Interpreter<CofreeJSON>(equal: CofreeJSON.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)

	guard let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON(a: aString, b: bString) }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) else { return nil }

	return JSON as String
}

_ = diffAndSerialize(a: "{\"hello\":\"world\",\"sup\":\"cat\"}", b: "{\"hello\":\"world\",\"sup\":\"dog\"}").map { print($0) }

if let aString = arguments[1].flatMap(data).flatMap(string), bString = arguments[2].flatMap(data).flatMap(string), JSON = diffAndSerialize(a: aString, b: bString) {
	print(JSON)
}
