public enum Diff: Comparable, CustomDebugStringConvertible, CustomDocConvertible {
	case Patch(Term, Term)
	indirect case Copy(Syntax<Diff, String>)

	public static func Insert(term: Term) -> Diff {
		return .Patch(.Empty, term)
	}

	public static func Delete(term: Term) -> Diff {
		return .Patch(term, .Empty)
	}

	public init(_ term: Term) {
		switch term {
		case let .Roll(s):
			self = .Copy(s.map(Diff.init))
		}
	}

	public var doc: Doc {
		switch self {
		case let .Patch(a, b):
			return Doc(a).bracket("{-", "-}")
				<> Doc(b).bracket("{+", "+}")
		case let .Copy(a):
			return a.doc
		}
	}

	public var debugDescription: String {
		switch self {
		case let .Patch(a, b):
			return ".Patch(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Copy(a):
			return ".Copy(\(String(reflecting: a)))"
		}
	}

	public var magnitude: Int {
		switch self {
		case .Patch:
			return 1
		case let .Copy(s):
			return s.map { $0.magnitude }.reduce(0, combine: +)
		}
	}

	public init(_ a: Term, _ b: Term) {
		if a == b {
			self = Diff(b)
			return
		}
		switch (a.syntax, b.syntax) {
		case let (.Leaf(v1), .Leaf(v2)) where v1 == v2:
			self = .Copy(.Leaf(v2))

		case let (.Branch(v1), .Branch(v2)):
			self = .Copy(.Branch(Diff.diff(v1, v2)))

		default:
			self = .Patch(a, b)
		}
	}

	public static func diff<C1: CollectionType, C2: CollectionType where C1.Index : RandomAccessIndexType, C1.Generator.Element == Term, C2.Index : RandomAccessIndexType, C2.Generator.Element == Term>(a: C1, _ b: C2) -> [Diff] {
		func magnitude(diffs: Stream<(Diff, Int)>) -> Int {
//			return diffs.first?.magnitude ?? 0
			return diffs.map { $1 }.reduce(0, combine: +)
		}

		func min<A>(a: A, _ rest: A..., _ isLessThan: (A, A) -> Bool) -> A {
			return rest.reduce(a, combine: {
				isLessThan($0, $1) ? $0 : $1
			})
		}

		func diff(a: Stream<Term>, _ b: Stream<Term>) -> Stream<(Diff, Int)> {
			switch (a, b) {
			case (.Nil, .Nil):
				return .Nil
			case (.Nil, .Cons):
				return b.map { (Diff.Insert($0), 1) }
			case (.Cons, .Nil):
				return a.map { (Diff.Delete($0), 1) }
			case let (.Cons(x, xs), .Cons(y, ys)):
				let copy = Diff(x, y)
				let here = Stream.Cons((copy, copy.magnitude), Memo { diff(xs.value, ys.value) })
				let insert = Stream.Cons((Diff.Insert(y), 1), Memo { diff(a, ys.value) })
				let delete = Stream.Cons((Diff.Delete(x), 1), Memo { diff(xs.value, b) })
				return min(here, insert, delete) {
					magnitude($0) < magnitude($1)
				}
			}
		}

		return Array(diff(Stream(sequence: a), Stream(sequence: b)).map { $0.0 })
	}
}
