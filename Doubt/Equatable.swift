func == (left: Fix, right: Fix) -> Bool {
	return left.out == right.out
}

func == <F: Equatable> (left: Syntax<F>, right: Syntax<F>) -> Bool {
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

func == (left: Diff, right: Diff) -> Bool {
	switch (left, right) {
	case let (.Patch(a1, b1), .Patch(a2, b2)):
		return a1 == a2 && b1 == b2
	case let (.Copy(a), .Copy(b)):
		return a() == b()
	default:
		return false
	}
}

func == <F: Equatable> (left: Doc<F>, right: Doc<F>) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Text(a), .Text(b)):
		return a == b
	case let (.Horizontal(a), .Horizontal(b)):
		return a == b
	case let (.Vertical(a), .Vertical(b)):
		return a == b
	case let (.Wrap(a1, b1, c1), .Wrap(a2, b2, c2)):
		return a1 == a2 && b1 == b2 && c1 == c2
	case let (.Join(s1, e1), .Join(s2, e2)):
		return s1 == s2 && e1 == e2
	default:
		return false
	}
}

func == (left: Pretty, right: Pretty) -> Bool {
	return true
}
