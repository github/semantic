struct RangedTerm {
	typealias Term = Cofree<String, Range<String.Index>>
	let term: Term
	let source: String
}

struct UnannotatedTerm {
	typealias Term = Cofree<String, ()>
	let term: Term

	static let indexed = (prefix: "[\n\t", separator: ",\n\t", suffix: "\n]")
	static let keyed = (prefix: "[\n\t", separator: ",\n\t", suffix: "\n}")

	var source: String {
		let indexed = UnannotatedTerm.indexed
		let keyed = UnannotatedTerm.keyed
		return term.cata {
			switch $0 {
			case let .Leaf(s):
				return s
			case let .Indexed(s):
				return indexed.prefix + s.joinWithSeparator(indexed.separator) + indexed.suffix
			case let .Keyed(s):
				return keyed.prefix + s.map { "\"\($0)\": \($1)" }.joinWithSeparator(keyed.separator) + keyed.suffix
			}
		}
	}

	var ranged: Cofree<String, Range<Int>> {
		let indexed = UnannotatedTerm.indexed
		let keyed = UnannotatedTerm.keyed
		return term.cata {
			switch $0 {
			case let .Leaf(s):
				return Cofree(0..<s.characters.count, .Leaf(s))
			case let .Indexed(i):
				var length = indexed.prefix.characters.count
				var results: [Cofree<String, Range<Int>>] = []
				for value in i {
					if results.count > 0 {
						length += indexed.separator.characters.count
					}
					results.append(value.map {
						($0.startIndex + length)..<($0.endIndex + length)
					})
					length += value.extract.count
				}
				return Cofree(0..<(length + indexed.suffix.characters.count), .Indexed(results))
			case let .Keyed(k):
				var length = keyed.prefix.characters.count
				var results: [(String, Cofree<String, Range<Int>>)] = []
				for (key, value) in k {
					if results.count > 0 {
						length += keyed.separator.characters.count
					}
					results.append((key, value.map {
						($0.startIndex + length)..<($0.endIndex + length)
					}))
					length += value.extract.count + 4 // for the characters around the key
				}
				return Cofree(0..<(length + keyed.suffix.characters.count), .Keyed(Dictionary(elements: results)))
			}
		}
	}

	var arranged: RangedTerm {
		let source = self.source
		return RangedTerm(
			term: ranged.map {
				source.startIndex.advancedBy($0.startIndex)..<source.startIndex.advancedBy($0.endIndex)
			},
			source: source)
	}
}


extension UnannotatedTerm: Arbitrary {
	static func arbitrary(k: Int) -> Gen<UnannotatedTerm> {
		let symbol: Gen<String> = Gen<Int>.choose((0, 15)).fmap { "_\($0)" }
		let leaf: Gen<Term> = symbol.fmap { Term((), .Leaf($0)) }
		let indexed: Gen<Term> = Gen<Int>.choose((0, k)).bind { n in
			sequence((0..<n).map { _ in arbitrary(k - 1) }).fmap {
				Term((), .Indexed($0.map { $0.term }))
			}
		}
		let keyed: Gen<Term> = Gen<Int>.choose((0, k)).bind { n in
			sequence((0..<n).map { _ in symbol.bind { key in Gen.pure(()).bind { arbitrary(k - 1) }.fmap { (key, $0.term) } } }).fmap {
				Term((), .Keyed(Dictionary(elements: $0)))
			}
		}
		return Gen.oneOf([
			leaf,
			indexed,
			keyed,
		]).fmap {
			UnannotatedTerm(term: $0)
		}
	}

	static var arbitrary: Gen<UnannotatedTerm> {
		return Gen.sized(arbitrary)
	}
}

extension RangedTerm: Arbitrary {
	static var arbitrary: Gen<RangedTerm> {
		return UnannotatedTerm.arbitrary.fmap { $0.arranged }
	}
}


@testable import Doubt
import Prelude
import SwiftCheck
