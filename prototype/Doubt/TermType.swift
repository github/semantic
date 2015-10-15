/// The type of terms.
public protocol TermType {
	typealias LeafType

	var unwrap: Syntax<Self, LeafType> { get }
}


extension TermType {
	/// Catamorphism over `TermType`s.
	///
	/// Folds the tree encoded by the receiver into a single value by recurring top-down through the tree, applying `transform` to leaves, then to branches, and so forth.
	public func cata<Result>(transform: Syntax<Result, LeafType> -> Result) -> Result {
		return self |> ({ $0.unwrap } >>> { $0.map { $0.cata(transform) } } >>> transform)
	}


	/// The size of the receiver.
	public var size: Int {
		return cata {
			switch $0 {
			case .Leaf:
				return 1
			case let .Indexed(i):
				return i.reduce(1, combine: +)
			case let .Keyed(k):
				return k.values.reduce(1, combine: +)
			}
		}
	}
}


extension Cofree: TermType {}


// MARK: - Equality

extension TermType {
	public static func equals(leaf: (LeafType, LeafType) -> Bool)(_ a: Self, _ b: Self) -> Bool {
		return Syntax.equals(ifLeaf: leaf, ifRecur: equals(leaf))(a.unwrap, b.unwrap)
	}
}


import Prelude
