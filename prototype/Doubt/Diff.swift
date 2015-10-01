public enum Diff: Comparable, CustomDebugStringConvertible, CustomDocConvertible, AlgebraicHashable {
	/// Replace a term with another term.
	case Patch(Term<Info>, Term<Info>)

	/// Copy a syntax node, recursively diffing its branches.
	indirect case Copy(Syntax<Diff, Info>)

	public static func Insert(term: Term<Info>) -> Diff {
		return .Patch(.Empty, term)
	}

	public static func Delete(term: Term<Info>) -> Diff {
		return .Patch(term, .Empty)
	}

	public init(_ term: Term<Info>) {
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

	public var hash: Hash {
		switch self {
		case let .Patch(a, b):
			return Hash("Patch", a.hash, b.hash)
		case let .Copy(syntax):
			return Hash("Copy", syntax.hash)
		}
	}

	public var magnitude: Int {
		func magnitude(syntax: Syntax<Diff, Info>) -> Int {
			return syntax.map { $0.magnitude }.reduce(0, combine: +)
		}
		switch self {
		case .Patch:
			return 1
		case let .Copy(s):
			return magnitude(s)
		}
	}

	public init(_ a: Term<Info>, _ b: Term<Info>) {
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

	public static func diff<C1: CollectionType, C2: CollectionType where C1.Index : RandomAccessIndexType, C1.Generator.Element == Term<Info>, C2.Index : RandomAccessIndexType, C2.Generator.Element == Term<Info>>(a: C1, _ b: C2) -> [Diff] {
		func magnitude(diffs: Stream<(Diff, Int)>) -> Int {
//			return diffs.first?.magnitude ?? 0
			return diffs.map { $1 }.reduce(0, combine: +)
		}

		func min<A>(a: A, _ rest: A..., _ isLessThan: (A, A) -> Bool) -> A {
			return rest.reduce(a, combine: {
				isLessThan($0, $1) ? $0 : $1
			})
		}

		func diff(a: Stream<Term<Info>>, _ b: Stream<Term<Info>>) -> Stream<(Diff, Int)> {
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


public func == (left: Diff, right: Diff) -> Bool {
	switch (left, right) {
	case let (.Patch(a1, b1), .Patch(a2, b2)):
		return a1 == a2 && b1 == b2
	case let (.Copy(a), .Copy(b)):
		return a == b
	default:
		return false
	}
}
