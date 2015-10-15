/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: TermType> {
	public typealias Diff = Free<Term.LeafType, Patch<Term>>

	public let equals: (Term, Term) -> Bool

	public func run(a: Term, _ b: Term) -> Diff {
		if equals(a, b) { return Diff(b) }
		let algorithm: Algorithm<Term, Diff>
		switch (a.unwrap, b.unwrap) {
		case let (.Keyed(a), .Keyed(b)):
			algorithm = .Roll(.ByKey(a, b, Syntax.Keyed >>> Diff.Roll >>> Algorithm.Pure))
		case let (.Indexed(a), .Indexed(b)):
			algorithm = .Roll(.ByIndex(a, b, Syntax.Indexed >>> Diff.Roll >>> Algorithm.Pure))
		default:
			algorithm = .Roll(.Recursive(a, b, Algorithm.Pure))
		}
		return Diff.Pure(.Replace(a, b))
	}
}


import Prelude
