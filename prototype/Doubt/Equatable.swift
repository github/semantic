public func == (left: Term, right: Term) -> Bool {
	return left.syntax == right.syntax
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Apply(a, aa), .Apply(b, bb)):
		return a == b && aa == bb
	case let (.Abstract(p1, b1), .Abstract(p2, b2)):
		return p1 == p2 && b1 == b2
	case let (.Assign(n1, v1), .Assign(n2, v2)):
		return n1 == n2 && v1 == v2
	case let (.Leaf(l1), .Leaf(l2)):
		return l1 == l2
	case let (.Group(n1, v1), .Group(n2, v2)):
		return n1 == n2 && v1 == v2
	default:
		return false
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

public func == (left: Doc, right: Doc) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty), (.Line, .Line):
		return true
	case let (.Text(a), .Text(b)):
		return a == b
	case let (.Nest(i, a), .Nest(j, b)):
		return i == j && a == b
	case let (.Concat(l1, r1), .Concat(l2, r2)):
		return l1 == l2 && r1 == r2
	case let (.Union(l1, r1), .Union(l2, r2)):
		return l1 == l2 && r1 == r2
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
