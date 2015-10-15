/// The type of terms.
public protocol TermType {
	typealias LeafType

	var unwrap: Syntax<Self, LeafType> { get }
}


extension Cofree: TermType {}


// MARK: - Equality

extension TermType {
	public static func equals(leaf: (LeafType, LeafType) -> Bool)(_ a: Self, _ b: Self) -> Bool {
		return Syntax.equals(ifLeaf: leaf, ifRecur: equals(leaf))(a.unwrap, b.unwrap)
	}
}


// MARK: - Equality

extension TermType {
	public static func equals(leaf: (LeafType, LeafType) -> Bool)(_ a: Self, _ b: Self) -> Bool {
		return Syntax.equals(ifLeaf: leaf, ifRecur: equals(leaf))(a.out, b.out)
	}
}
