import Assertions
import Doubt
import Prelude
import Madness
import XCTest

final class JSONParserTests: XCTestCase {
	func testCapturesKeysInKeyedElements() {
		let dictWithArray = "{ \"hello\":\n    [\"world\",\n    \"sailor\"\n  ]}"
		let array: Cofree<JSONLeaf, Range<Int>> = Cofree(15..<41, .Indexed([
				Cofree(16..<23, .Leaf(.String("world"))),
				Cofree(29..<37, .Leaf(.String("sailor")))
			]))
		
		let fixedPairs: Cofree<JSONLeaf, Range<Int>> = Cofree(2..<41, .Fixed([Cofree(2..<9, .Leaf(.String("hello"))), array]))

		let expected: Cofree<JSONLeaf, Range<Int>> = Cofree(0..<42, .Keyed(["hello": fixedPairs]))
		let actual = Madness.parse(json, input: dictWithArray).right!
		let startRange = actual.extract.2
		let new: Cofree<JSONLeaf, Range<Int>> = actual.map({ tuple in
			let startI: Int = startRange.startIndex.distanceTo(tuple.2.startIndex)
			let endI: Int = startRange.startIndex.distanceTo(tuple.2.endIndex)
			return Range(start: startI, end: endI)
		})
		
		assert(new, == , expected)
	}
}
