public func == (left: Term, right: Term) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Roll(s), .Roll(t)):
		return s == t

	default:
		return false
	}
}

public func == <F: Equatable> (left: Syntax<F>, right: Syntax<F>) -> Bool {
	switch (left, right) {
	case let (.Apply(a, aa), .Apply(b, bb)):
		return a == b && aa == bb
	case let (.Abstract(p1, b1), .Abstract(p2, b2)):
		return p1 == p2 && b1 == b2
	case let (.Assign(n1, v1), .Assign(n2, v2)):
		return n1 == n2 && v1 == v2
	case let (.Variable(n1), .Variable(n2)):
		return n1 == n2
	case let (.Literal(l1), .Literal(l2)):
		return l1 == l2
	case let (.Group(n1, v1), .Group(n2, v2)):
		return n1 == n2 && v1 == v2
	default:
		return false
	}
}

public func == (left: Diff, right: Diff) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Patch(a1, b1), .Patch(a2, b2)):
		return a1 == a2 && b1 == b2
	case let (.Copy(a), .Copy(b)):
		return a == b
	default:
		return false
	}
}

public func == (left: Doc, right: Doc) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Text(a, x), .Text(b, y)):
		return a == b && x == y
	case let (.Line(i, x), .Line(j, y)):
		return i == j && x == y
	default:
		return false
	}
}

public func == <A: Equatable> (left: Vertex<A>, right: Vertex<A>) -> Bool {
	switch (left, right) {
	case (.End, .End):
		return true
	case let (.XY(a, x1, y1), .XY(b, x2, y2)):
		return a == b && x1.value == x2.value && y1.value == y2.value
	default:
		return false
	}
}
