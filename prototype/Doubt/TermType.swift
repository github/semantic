/// The type of terms.
public protocol TermType {
	typealias Leaf

	var unwrap: Syntax<Self, Leaf> { get }
}


extension TermType {
	/// Catamorphism over `TermType`s.
	///
	/// Folds the tree encoded by the receiver into a single value by recurring top-down through the tree, applying `transform` to leaves, then to branches, and so forth.
	public func cata<Result>(transform: Syntax<Result, Leaf> -> Result) -> Result {
		return self |> ({ $0.unwrap } >>> { $0.map { $0.cata(transform) } } >>> transform)
	}

	/// Paramorphism over `TermType`s.
	///
	/// Folds the tree encoded by the receiver into a single value by recurring top-down through the tree, applying `transform` to leaves, then to branches, and so forth. Each recursive instance is made available in the `Syntax` alongside the result value at that node.
	public func para<Result>(transform: Syntax<(Self, Result), Leaf> -> Result) -> Result {
		return self |> ({ $0.unwrap } >>> { $0.map { ($0, $0.para(transform)) } } >>> transform)
	}


	/// The count of nodes in the receiver.
	///
	/// This is used to compute the cost of patches, such that a patch inserting a very large tree will be charged approximately the same as a very large tree consisting of many small patches.
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
	public static func equals(leaf: (Leaf, Leaf) -> Bool)(_ a: Self, _ b: Self) -> Bool {
		return Syntax.equals(ifLeaf: leaf, ifRecur: equals(leaf))(a.unwrap, b.unwrap)
	}
}


import Prelude
