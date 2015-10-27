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
		let firstIndex = actual.extract
		let new: Cofree<JSONLeaf, Range<Int>> = actual.map({ range in
			let startI: Int = firstIndex.startIndex.distanceTo(range.startIndex)
			let endI: Int = firstIndex.startIndex.distanceTo(range.endIndex)
			return Range(start: startI, end: endI)
		})
		
		assert(new, == , expected)
	}
}
