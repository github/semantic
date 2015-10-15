/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: TermType> {
	public let equals: (Term, Term) -> Bool

	public func run() {}
}
