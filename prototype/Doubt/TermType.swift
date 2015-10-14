/// The type of terms.
public protocol TermType {
	typealias LeafType

	var out: Syntax<Self, LeafType> { get }
}
