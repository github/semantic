public enum Diff: Comparable, CustomDebugStringConvertible, CustomDocConvertible {
	case Empty
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
		case .Empty:
			self = .Empty
		case let .Roll(s):
			self = .Copy(s.map(Diff.init))
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Patch(a, b):
			return .Horizontal([
				.Wrap(.Text("{-"), Doc(a), .Text("-}")),
				.Wrap(.Text("{+"), Doc(b), .Text("+}"))
			])
		case let .Copy(a):
			return a.doc
		}
	}

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Patch(a, b):
			return ".Patch(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Copy(a):
			return ".Copy(\(String(reflecting: a)))"
		}
	}

	public var magnitude: Int {
		switch self {
		case .Empty:
			return 0
		case .Patch:
			return 1
		case let .Copy(s):
			return s.map { $0.magnitude }.reduce(0, combine: +)
		}
	}

	public init(_ a: Term, _ b: Term) {
		switch (a, b) {
		case (.Empty, .Empty):
			self = .Empty

		case let (.Roll(a), .Roll(b)):
			switch (a, b) {
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
				self = .Patch(Term(a), Term(b))
			}

		default:
			self = Patch(a, b)
		}
	}

	public static func diff<C1: CollectionType, C2: CollectionType where C1.Index : RandomAccessIndexType, C1.Generator.Element == Term, C2.Index : RandomAccessIndexType, C2.Generator.Element == Term>(a: C1, _ b: C2) -> [Diff] {
		func magnitude(diffs: Stream<(Diff, Int)>) -> Int {
			return diffs.first?.1 ?? 0
			// fixme; this should actually consider the rest of the diff, but we aren’t memoizing that correctly.
//			return diffs.map { $1 }.fold(0, combine: { $0 + $1.value })
		}

		func min<A>(a: A, _ rest: A..., _ isLessThan: (A, A) -> Bool) -> A {
			return rest.reduce(a) { isLessThan($0, $1) ? $0 : $1 }
		}

		let graph = Vertex<(Term, Term, Diff, Int)>(rows: Stream(sequence: a), columns: Stream(sequence: b)) {
			let diff = Diff($0, $1)
			return ($0, $1, diff, diff.magnitude)
		}

		func diff(vertex: Vertex<(Term, Term, Diff, Int)>) -> Stream<(Diff, Int)> {
			switch vertex {
			case .End:
				return .Nil

			case let .XY((a, b, copy, copyCost), x, y):
				let right = x.value
				let down = y.value
				switch (right, down) {
				case (.End, .End):
					return .Cons((copy, copyCost), Memo(evaluated: .Nil))
				case (.End, .XY):
					return vertex.row.map { (Diff.Delete($0.0), 1) }
				case (.XY, .End):
					return vertex.column.map { (Diff.Insert($0.0), 1) }
				case (.XY, .XY):
					let copy = Stream.Cons((copy, copyCost), vertex.diagonal.map(diff))
					let insert = Stream.Cons((Diff.Insert(b), 1), x.map(diff))
					let delete = Stream.Cons((Diff.Delete(a), 1), y.map(diff))
					return min(copy, insert, delete) {
						magnitude($0) < magnitude($1)
					}
				}
			}
		}

		return Array(diff(graph).map { $0.0 })
	}
}
