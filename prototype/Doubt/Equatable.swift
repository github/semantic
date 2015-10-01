public func == <A: Equatable> (left: Term<A>, right: Term<A>) -> Bool {
	return Syntax.equals(==)(left.syntax, right.syntax)
}

public func == <F: Equatable, A: Equatable> (left: Syntax<F, A>, right: Syntax<F, A>) -> Bool {
	return Syntax.equals(==)(left, right)
}
