final class VertexTests: XCTestCase {
	func testConstructingWithStreamsProducesMatrixOfProducts() {
		let actual = Vertex(rows: Stream(sequence: [ "a", "b" ]), columns: Stream(sequence: [ "0", "1" ]), combine: +)

		let expected: Vertex<String> = {
			let end = Memo<Vertex<String>>(evaluated: .End)
			let b1 = Memo<Vertex<String>> { .XY("b1", end, end) }
			let b0 = Memo<Vertex<String>> { .XY("b0", b1, end) }
			let a1 = Memo<Vertex<String>> { .XY("a1", end, b1) }
			let a0 = Memo<Vertex<String>> { .XY("a0", a1, b0) }
			return a0.value
		}()
		XCTAssert(actual == expected, "\(String(reflecting: actual)) != \(String(reflecting: expected))")
	}

	func testConstructingWithStreamsProducesMatrix() {
		let actual = Vertex(rows: Stream(sequence: [ "a", "b", "c" ]), columns: Stream(sequence: [ "0", "1", "2" ]), combine: +)

		let expected: Vertex<String> = {
			let end = Memo<Vertex<String>>(evaluated: .End)
			let c2 = Memo<Vertex<String>> { .XY("c2", end, end) }
			let c1 = Memo<Vertex<String>> { .XY("c1", c2, end) }
			let c0 = Memo<Vertex<String>> { .XY("c0", c1, end) }
			let b2 = Memo<Vertex<String>> { .XY("b2", end, c2) }
			let b1 = Memo<Vertex<String>> { .XY("b1", b2, c1) }
			let b0 = Memo<Vertex<String>> { .XY("b0", b1, c0) }
			let a2 = Memo<Vertex<String>> { .XY("a2", end, b2) }
			let a1 = Memo<Vertex<String>> { .XY("a1", a2, b1) }
			let a0 = Memo<Vertex<String>> { .XY("a0", a1, b0) }
			return a0.value
		}()
		XCTAssert(actual == expected, "\(String(reflecting: actual)) != \(String(reflecting: expected))")
	}
}


@testable import Doubt
import XCTest
