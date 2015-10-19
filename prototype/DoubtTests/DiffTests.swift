final class DiffTests: XCTestCase {
	override static func setUp() {
		sranddev()
	}

	typealias Term = RangedTerm.Term
	typealias Diff = Free<String, Patch<Term>>

	let interpreter = Interpreter<Term>(equal: ==, comparable: const(true), cost: Diff.sum(const(1)))

	func testEqualTermsProduceIdentityDiffs() {
		property("equal terms produce identity diffs") <- forAll { (term: RangedTerm) in
			Diff.sum(const(1))(self.interpreter.run(term.term, term.term)) == 0
		}
	}

	func testInequalTermsProduceNonIdentityDiffs() {
		property("inequal terms produce non-identity diffs") <- forAll { (diff: RangedDiff) in
			(!Term.equals(annotation: const(true), leaf: ==)(diff.a.term, diff.b.term)) ==> Diff.sum(const(1))(diff.diff) > 0
		}
	}

	func testEqualityIsReflexive() {
		property("equality is reflexive") <- forAll { (diff: RangedDiff) in
			equal(diff.diff, diff.diff)
		}
	}

	func testDoubleInversionIsIdempotent() {
		property("double inversion is idempotent") <- forAll { (diff: RangedDiff) in
			equal(diff.diff, diff.diff.inverse.inverse)
		}
	}

	func testOriginalTermsAreRecoverable() {
		let equal = Cofree<String, ()>.equals(annotation: const(true), leaf: ==)
		property("before state is recoverable") <- forAll { (diff: RangedDiff) in
			diff.diff.map { $0.map { $0.map(const(())) } }.before.map {
				equal($0, diff.a.term.map(const(())))
			} ?? false
		}

		property("after state is recoverable") <- forAll { (diff: RangedDiff) in
			diff.diff.map { $0.map { $0.map(const(())) } }.after.map {
				equal($0, diff.b.term.map(const(())))
			} ?? false
		}
	}
}


private func equal(a: DiffTests.Diff, _ b: DiffTests.Diff) -> Bool {
	return Free.equals(ifPure: Patch.equals(Cofree.equals(annotation: ==, leaf: ==)), ifRoll: ==)(a, b)
}


@testable import Doubt
import Prelude
import SwiftCheck
import XCTest
