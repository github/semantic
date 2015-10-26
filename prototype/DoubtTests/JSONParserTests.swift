import Assertions
import Doubt
import Prelude
import Madness
import XCTest

final class JSONParserTests: XCTestCase {
	func testCapturesKeysInKeyedElements() {
		let dictWithArray = "{\"hello\": [\"world\"],\"sup\": [\"cat\", \"dog\", \"keith\"] }"
		
		let expected: Cofree<JSONLeaf, Range<String.CharacterView.Index>> = Cofree(0..<61, .Keyed([
			"hello": Cofree(11..<42, .Indexed([
				Cofree(17..<24, "world"),
				Cofree(30..<38, "sailor")
			])),
			"goodbye": Cofree(57..<59, .Indexed([]))
		]))

//		assert(Madness.parse(Doubt.json, input: dictWithArray).right, == , )
	}
