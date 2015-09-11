public enum Diff: CustomDocConvertible, Equatable {
	case Patch(Syntax<Fix>, Syntax<Fix>)
	case Copy(() -> Syntax<Diff>)

	public var doc: Doc<Pretty> {
		switch self {
		case let .Patch(a, b):
			return .Horizontal([
				.Wrap(Pretty.Text("{-"), Pretty(a.doc), Pretty.Text("-}")),
				.Wrap(Pretty.Text("{+"), Pretty(b.doc), Pretty.Text("+}"))
			])
		case let .Copy(a):
			return a().doc
		}
	}

	public var description: String {
		switch self {
		case let .Patch(a, b):
			return ".Patch(\(a), \(b))"
		case let .Copy(a):
			return ".Copy(\(a()))"
		}
	}

	public init(_ a: Syntax<Fix>, _ b: Syntax<Fix>) {
		switch (a, b) {
		case let (.Apply(a, aa), .Apply(b, bb)):
			self = .Copy({ .Apply(Diff(a.out, b.out), Array(zip(aa, bb).lazy.map { ($0.out, $1.out) }.map(Diff.init))) })

		case let (.Abstract(p1, b1), .Abstract(p2, b2)):
			self = .Copy({ .Abstract(Array(zip(p1, p2).lazy.map { ($0.out, $1.out) }.map(Diff.init)), Diff(b1.out, b2.out)) })

		case let (.Assign(n1, v1), .Assign(n2, v2)) where n1 == n2:
			self = .Copy({ .Assign(n2, Diff(v1.out, v2.out)) })

		case let (.Variable(n1), .Variable(n2)) where n1 == n2:
			self = .Copy({ .Variable(n2) })

		case let (.Literal(v1), .Literal(v2)) where v1 == v2:
			self = .Copy({ .Literal(v2) })

		case let (.Group(n1, v1), .Group(n2, v2)):
			self = .Copy({ .Group(Diff(n1.out, n2.out), Array(zip(v1, v2).lazy.map { ($0.out, $1.out) }.map(Diff.init))) })

		default:
			self = .Patch(a, b)
		}
	}
}

