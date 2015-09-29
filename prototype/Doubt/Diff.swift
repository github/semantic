public enum Diff: Comparable, CustomDebugStringConvertible, CustomDocConvertible {
	case Patch(Term, Term)
	indirect case Copy(Syntax<Diff>)

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
		switch (a.syntax, b.syntax) {
		case let (.Apply(a, aa), .Apply(b, bb)):
			self = .Copy(.Apply(Diff(a, b), Diff.diff(aa, bb)))

		case let (.Abstract(p1, b1), .Abstract(p2, b2)):
			self = .Copy(.Abstract(Diff.diff(p1, p2), Diff.diff(b1, b2)))

		case let (.Assign(n1, v1), .Assign(n2, v2)) where n1 == n2:
			self = .Copy(.Assign(n2, Diff(v1, v2)))

		case let (.Variable(n1), .Variable(n2)) where n1 == n2:
			self = .Copy(.Variable(n2))

		case let (.Literal(v1), .Literal(v2)) where v1 == v2:
			self = .Copy(.Literal(v2))

		case let (.Group(n1, v1), .Group(n2, v2)):
			self = .Copy(.Group(Diff(n1, n2), Diff.diff(v1, v2)))

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
