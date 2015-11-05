final class LocationTests: XCTestCase {
}


private let leaf = Cofree(1, .Leaf("a string"))
private let term: Cofree<String, Int> = Cofree(0, .Indexed([
	leaf,
	Cofree(2, .Leaf("b string")),
	Cofree(3, .Keyed([
		"a": Cofree(4, .Leaf("a nested string")),
		"b": Cofree(5, .Leaf("b nested string")),
	])),
]))


@testable import Doubt
import XCTest
