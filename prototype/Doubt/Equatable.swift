public func == <A: Equatable> (left: Term<A>, right: Term<A>) -> Bool {
	return equals(left.syntax, right.syntax, ==)
}

private func equals<F, A: Equatable>(left: Syntax<F, A>, _ right: Syntax<F, A>, _ recur: (F, F) -> Bool) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Leaf(l1), .Leaf(l2)):
		return l1 == l2
	case let (.Branch(v1), .Branch(v2)):
		return v1.count == v2.count && zip(v1, v2).reduce(true) { $0 && recur($1.0, $1.1) }
	default:
		return false
	}
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return equals(left, right, ==)
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
