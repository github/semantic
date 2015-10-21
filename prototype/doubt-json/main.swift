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

let readFile = { (path: String) -> String? in
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

if let a = arguments[1].flatMap(readFile).flatMap({
		curry(parse)(json)($0).right
	}), b = arguments[2].flatMap(readFile).flatMap({
		curry(parse)(json)($0).right
	}) {
	let diff = Interpreter(comparable: const(true), cost: Diff.sum(Patch.difference)).run(a, b)
	print(diff)
}
