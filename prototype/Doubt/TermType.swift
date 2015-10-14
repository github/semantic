/// The type of terms.
public protocol TermType {
	typealias LeafType

	var out: Syntax<Self, LeafType> { get }
}


extension Fix: TermType {}

extension Cofree: TermType {
	public var out: Syntax<Cofree, A> {
		return unwrap
	}
}
