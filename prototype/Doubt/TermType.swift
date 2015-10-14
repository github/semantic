/// The type of terms.
public protocol TermType {
	typealias LeafType

	var out: Syntax<Self, LeafType> { get }
}


extension TermType {
	public func cata<Result>(transform: Syntax<Result, LeafType> -> Result) -> Result {
		return self |> ({ $0.out } >>> { $0.map { $0.cata(transform) } } >>> transform)
	}
}


extension Fix: TermType {}

extension Cofree: TermType {
	public var out: Syntax<Cofree, A> {
		return unwrap
	}
}


import Prelude
