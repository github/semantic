final class AlgorithmTests: XCTestCase {

}


private typealias Term = Cofree<String, Int>
private typealias Diff = Free<String, Patch<Term>>

private let a = Cofree(0, [ Cofree(1, .Leaf("a")), Cofree(2, .Leaf("b")), Cofree(3, .Leaf("c")) ])
private let b = Cofree(0, [ Cofree(1, .Leaf("c")), Cofree(2, .Leaf("b")), Cofree(3, .Leaf("a")) ])


import Assertions
@testable import Doubt
import XCTest
