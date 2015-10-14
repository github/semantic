final class AlgorithmTests: XCTestCase {

}


private typealias Term = Cofree<String, Int>
private typealias Diff = Free<String, Patch<Term>>

private let a = Term(0, [ Term(1, .Leaf("a")), Term(2, .Leaf("b")), Term(3, .Leaf("c")) ])
private let b = Term(0, [ Term(1, .Leaf("c")), Term(2, .Leaf("b")), Term(3, .Leaf("a")) ])


import Assertions
@testable import Doubt
import XCTest
