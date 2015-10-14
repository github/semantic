/// The type of terms.
public protocol TermType {
	typealias LeafType

	var out: Syntax<Self, LeafType> { get }
}


extension TermType {
	public func cata<Result>(transform: Syntax<Result, LeafType> -> Result) -> Result {
		return self |> ({ $0.out } >>> { $0.map { $0.cata(transform) } } >>> transform)
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


extension Fix: TermType {}

extension Cofree: TermType {
	public var out: Syntax<Cofree, A> {
		return unwrap
	}
}


import Prelude
