/// The fixpoint of `Syntax`.
///
/// `Syntax` is a non-recursive type parameterized by the type of its child nodes. Instantiating it to `Fix` makes it into a recursive tree by “tying the knot”—each child node of `Syntax<Fix, A>` is represented by a `Fix` which in turn contains a `Syntax<Fix, A>`. So in the same way that the `fix` function allows one to tie a non-recursive function into a recursive one, `Fix` allows one to tie a non-recursive type into a recursive one. Unfortunately, due to Swift’s lack of higher-rank types, this cannot currently be abstracted over the type which is made recursive, and thus it is hard-coded to `Syntax<Fix, A>` rather than provided by a type parameter `F` applied to `Fix<F>`.
public enum Fix<A>: CustomDebugStringConvertible, CustomDocConvertible {
	/// A recursive instantiation of `Syntax`, unrolling another iteration of the recursive type.
	indirect case In(Syntax<Fix, A>)

	public var out: Syntax<Fix, A> {
		switch self {
		case let .In(s):
			return s
		}
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		return ".In(\(String(reflecting: out)))"
	}


	// MARK: CustomDocConvertible

	public var doc: Doc {
		return out.doc
	}
}


// MARK: - Equality

extension Fix {
	public static func equals(param: (A, A) -> Bool)(_ left: Fix, _ right: Fix) -> Bool {
		return Syntax.equals(ifLeaf: param, ifRecur: equals(param))(left.out, right.out)
	}
}

public func == <A: Equatable> (left: Fix<A>, right: Fix<A>) -> Bool {
	return Fix.equals(==)(left, right)
}


// MARK: - Hashing

extension Fix {
	public func hash(param: A -> Hash) -> Hash {
		return out.hash(ifLeaf: param, ifRecur: { $0.hash(param) })
	}
}

extension Fix where A: Hashable {
	public var hash: Hash {
		return hash(Hash.init)
	}
}


// MARK: JSONConvertible

extension Fix {
	public func JSON(ifLeaf: A -> Doubt.JSON) -> Doubt.JSON {
		return out.JSON(ifLeaf: ifLeaf, ifRecur: { $0.JSON(ifLeaf) })
	}
}
