struct UnannotatedTerm {
	typealias Term = Cofree<String, ()>
	let term: Term

	static let indexed = (prefix: "[\n\t", separator: ",\n\t", suffix: "\n]")
	static let fixed = (prefix: "(\n\t", separator: ",\n\t", suffix: "\n)")
	static let keyed = (prefix: "[\n\t", separator: ",\n\t", suffix: "\n}")

	var source: String {
		let indexed = UnannotatedTerm.indexed
		let keyed = UnannotatedTerm.keyed
		let fixed = UnannotatedTerm.fixed
		return term.cata {
			switch $0 {
			case let .Leaf(s):
				return s
			case let .Indexed(s):
				return indexed.prefix + s.joinWithSeparator(indexed.separator) + indexed.suffix
			case let .Fixed(s):
				return fixed.prefix + s.joinWithSeparator(fixed.separator) + fixed.suffix
			case let .Keyed(s):
				return keyed.prefix + s.map { "\"\($0)\": \($1)" }.joinWithSeparator(keyed.separator) + keyed.suffix
			}
		}
	}

	var ranged: Cofree<String, Range<Int>> {
		let indexed = UnannotatedTerm.indexed
		let keyed = UnannotatedTerm.keyed
		let fixed = UnannotatedTerm.fixed
		
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
			case let .Fixed(i):
				var length = fixed.prefix.characters.count
				var results: [Cofree<String, Range<Int>>] = []
				for value in i {
					if results.count > 0 {
						length += fixed.separator.characters.count
					}
					results.append(value.map {
						($0.startIndex + length)..<($0.endIndex + length)
					})
					length += value.extract.count
				}
				return Cofree(0..<(length + fixed.suffix.characters.count), .Fixed(results))
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
		]).fmap(UnannotatedTerm.init)
	}

	static var arbitrary: Gen<UnannotatedTerm> {
		return arbitrary(4)
	}


	static func shrink(term: UnannotatedTerm) -> [UnannotatedTerm] {
		let equal = Term.equals(annotation: const(true), leaf: ==)
		/// A smaller-than-or-equal-to permutation of `term`. Shrinking is performed outward-in by dropping elements from branches.
		let shrunk: UnannotatedTerm.Term = term.term.para {
			switch $0 {
			case let .Leaf(a):
				return Cofree((), .Leaf(a))
			case let .Indexed(i):
				return Cofree((), .Indexed(i.reduce(true) { $0 && equal($1) }
					? i.dropLast().map { $1 }
					: i.map { $1 }))
			case let .Fixed(i):
				return Cofree((), .Fixed(i.reduce(true) { $0 && equal($1) }
					? i.dropLast().map { $1 }
					: i.map { $1 }))
			case let .Keyed(k):
				return Cofree((), .Keyed(Dictionary(elements: k.reduce(true) { $0 && equal($1.1) }
					? k.dropLast().map { ($0, $1.1) }
					: k.map { ($0, $1.1) })))
			}
		}
		/// If the permutation is unchanged, we cannot shrink any further.
		return equal(term.term, shrunk)
			? []
			: [ UnannotatedTerm(term: shrunk) ]
	}
}


import Doubt
import Prelude
import SwiftCheck
