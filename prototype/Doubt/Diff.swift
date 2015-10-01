public enum Diff: Comparable, CustomDebugStringConvertible, CustomDocConvertible, AlgebraicHashable {
	/// Replace a term with another term.
	case Patch(Term<Info>, Term<Info>)

	/// Copy a syntax node, recursively diffing its branches.
	indirect case Copy(Syntax<Diff, Info>)

	/// Insert, remove, and patch terms by some assigned identity.
	indirect case ByKey([String:Term<Info>], [String:Term<Info>])

	/// Insert, remove, and patch terms by index. This computes the SES (or some approximation thereof).
	indirect case ByIndex([Term<Info>], [Term<Info>])

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
		case let .ByKey(a, b):
			return a.keys.sort().map { Doc($0) <> Doc(":") <+> Doc(a[$0]!) }.joinWithSeparator(",").bracket("{-", "-}")
				<> b.keys.sort().map { Doc($0) <> Doc(":") <+> Doc(b[$0]!) }.joinWithSeparator(",").bracket("{+", "+}")
		case let .ByIndex(a, b):
			return a.map(Doc.init).joinWithSeparator(",").bracket("{-", "-}")
				<> b.map(Doc.init).joinWithSeparator(",").bracket("{+", "+}")
		}
	}

	public var debugDescription: String {
		switch self {
		case let .Patch(a, b):
			return ".Patch(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Copy(a):
			return ".Copy(\(String(reflecting: a)))"
		case let .ByKey(a, b):
			return ".ByKey(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .ByIndex(a, b):
			return ".ByIndex(\(String(reflecting: a)), \(String(reflecting: b)))"
		}
	}

	public var hash: Hash {
		switch self {
		case let .Patch(a, b):
			return Hash("Patch", a.hash, b.hash)
		case let .Copy(syntax):
			return Hash("Copy", syntax.hash)
		case let .ByKey(a, b):
			return Hash("ByKey", .Unordered(a.map { Hash($0, $1.hash) }), .Unordered(b.map { Hash($0, $1.hash) }))
		case let .ByIndex(a, b):
			return Hash("ByIndex", .Ordered(a.map { $0.hash }), .Ordered(b.map { $0.hash }))
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
		case let .ByKey(a, b):
			let deleted = Set(a.keys).subtract(b.keys)
			let inserted = Set(b.keys).subtract(a.keys)
			let diffed = Set(a.keys).intersect(b.keys).map {
				Diff(a[$0]!, b[$0]!).magnitude
			}
			return deleted.count + inserted.count + diffed.reduce(0, combine: +)
		case let .ByIndex(a, b):
			// fixme: SES 😞
			return 0
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
	case let (.ByKey(a1, b1), .ByKey(a2, b2)):
		return a1 == a2 && b1 == b2
	case let (.ByIndex(a1, b1), .ByIndex(a2, b2)):
		return a1 == a2 && b1 == b2
	default:
		return false
	}
}
