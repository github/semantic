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

if let a = arguments[1].flatMap(JSON.init), b = arguments[2].flatMap(JSON.init) {
	let diff = Interpreter(comparable: const(true), cost: Diff.sum(Patch.difference)).run(a.term, b.term)
	if let JSON = NSString(data: diff.JSON(ifPure: { $0.JSON { $0.JSON } }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) {
		print(JSON)
	}
}
