import Cocoa
import Doubt
import Either
import Prelude
import Madness

func benchmark<T>(label: String? = nil, _ f: () -> T) -> T {
	let start = NSDate.timeIntervalSinceReferenceDate()
	let result = f()
	let end = NSDate.timeIntervalSinceReferenceDate()
	print((label.map { "\($0): " } ?? "") + "\(end - start)s")
	return result
}

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
	let aParsed = benchmark("parsing a") { curry(parse)(json)(aString) }
	guard let a = aParsed.right else {
		_ = aParsed.left.map { print("error parsing a:", $0) }
		return nil
	}
	let bParsed = benchmark("parsing b") { curry(parse)(json)(bString) }
	guard let b = bParsed.right else {
		_ = bParsed.left.map { print("error parsing b:", $0) }
		return nil
	}

	let diff = benchmark("diffing a & b") {
		Interpreter<CofreeJSON>(equal: CofreeJSON.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)
	}

	return benchmark("JSON") {
		NSString(data: diff.JSON(ifPure: { $0.JSON(a: aString, b: bString) }, ifLeaf: { $0.JSON }).serialize(), encoding: NSUTF8StringEncoding) as String?
	}
}

let readFile = { (path: String) -> String? in
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

if let a = arguments[1].flatMap(readFile), b = arguments[2].flatMap(readFile), diff = diffAndSerialize(a: a, b: b) {
	print(diff)
}
