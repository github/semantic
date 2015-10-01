public func == <A: Equatable> (left: Term<A>, right: Term<A>) -> Bool {
	return Syntax.equals(==)(left.syntax, right.syntax)
}
