/// The type of terms.
public protocol TermType {
	typealias LeafType

	var out: Syntax<Self, LeafType> { get }
}


extension Cofree: TermType {
	public var out: Syntax<Cofree, A> {
		return unwrap
	}
}


// MARK: - Equality

extension TermType {
	public static func equals(leaf: (LeafType, LeafType) -> Bool)(_ a: Self, _ b: Self) -> Bool {
		return Syntax.equals(ifLeaf: leaf, ifRecur: equals(leaf))(a.out, b.out)
	}
}
