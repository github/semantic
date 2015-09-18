final class DoubtTests: XCTestCase {
	func testEqualSyntaxResultsInRecursivelyCopyingDiff() {
		if let s = sexpr("\t(\n( a) \n)\t")?.value, t = sexpr("((a))")?.value {
			XCTAssertEqual(Diff(s, t), Diff.Copy(.Apply(.Copy(.Apply(.Copy(.Variable("a")), [])), [])))
		}
	}
}

let atom = Syntax<Term>.Variable <^> ^("abcdefghijklmnopqrstuvwxyz".characters.map { String($0) })
let ws = ^(" \t\n".characters.map { String($0) })

let sexpr: String -> State<Term>? = fix { sexpr in
	let list = Syntax<Term>.Apply <^> (ws* *> ^"(" *> ws* *> sexpr <*> sexpr* <* ^")")
	return Term.init <^> (atom <|> list) <* ws*
}


@testable import Doubt
import XCTest
