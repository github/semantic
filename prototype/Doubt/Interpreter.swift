/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: TermType> {
	public typealias Diff = Free<Term.LeafType, Patch<Term>>

	public let equals: (Term, Term) -> Bool

	public func run(a: Term, _ b: Term) -> Diff {
		if equals(a, b) { return Diff(b) }
		return Diff.Pure(.Replace(a, b))
	}
}
