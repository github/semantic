//  Copyright © 2015 GitHub. All rights reserved.

/// The cofree comonad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the comonad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `B` paired with it) to satisfy the comonad laws.
///
/// This type is dual to `Free`. Where `Free` is inhabited by syntax trees where some terms are replaced with `B`s, `Cofree` is inhabited by syntax trees where all terms are annotated with `B`s. In Doubt, this allows us to e.g. annotate terms with source range information, categorization, etc.
public enum Cofree<A, B> {
	case Unroll(B, () -> Syntax<Cofree, A>)

	public var unwrap: Syntax<Cofree, A> {
		switch self {
		case let .Unroll(_, rest):
			return rest()
		}
	}


	/// Recursively copies a `Fix<A>` into a `Cofree<A, B>` with a function assigning `B` for every `Fix<A>`.
	public init(_ fix: Fix<A>, _ annotate: Fix<A> -> B) {
		self = Cofree<A, Fix<A>>.coiterate { $0.out } (fix).map(annotate)
	}


	/// Constructs a cofree by coiteration.
	///
	/// The initial seed is used as the annotation of the returned value. The continuation of the structure is unpacked by applying `annotate` to the seed and mapping the resulting syntax’s values recursively. In this manner, the structure is unfolded bottom-up, starting with `seed` and ending at the leaves.
	///
	/// As this is the dual of `Free.iterate`, it’s unsurprising that we have a similar guarantee: coiteration is linear in the size of the constructed tree.
	public static func coiterate(annotate: B -> Syntax<B, A>)(_ seed: B) -> Cofree {
		return .Unroll(seed, { annotate(seed).map(coiterate(annotate)) })
	}
}


// MARK: - Functor

extension Cofree {
	public func map<Other>(transform: B -> Other) -> Cofree<A, Other> {
		return .Unroll(transform(extract), { self.unwrap.map { $0.map(transform) } })
	}
}


// MARK: - Comonad

extension Cofree {
	/// Returns the value annotating the syntax tree at this node.
	public var extract: B {
		switch self {
		case let .Unroll(b, _):
			return b
		}
	}

	/// Returns a new `Cofree` by recursively applying `transform` to each node, producing the annotations for the copy.
	public func extend<Other>(transform: Cofree -> Other) -> Cofree<A, Other> {
		return .Unroll(transform(self), { self.unwrap.map { $0.extend(transform) } })
	}

	/// Returns a new `Cofree` constructed by recursively annotating each subtree with itself.
	public var duplicate: Cofree<A, Cofree<A, B>> {
		return extend(id)
	}
}


// MARK: - Equality

extension Cofree {
	public static func equals(annotation annotation: (B, B) -> Bool, leaf: (A, A) -> Bool)(_ left: Cofree, _ right: Cofree) -> Bool {
		return annotation(left.extract, right.extract)
			&& Syntax.equals(ifLeaf: leaf, ifRecur: Cofree.equals(annotation: annotation, leaf: leaf))(left.unwrap, right.unwrap)
	}
}

public func == <A: Equatable, B: Equatable> (left: Cofree<A, B>, right: Cofree<A, B>) -> Bool {
	return Cofree.equals(annotation: ==, leaf: ==)(left, right)
}


// MARK: - Hashing

extension Cofree {
	public func hash(annotation annotation: B -> Hash, leaf: A -> Hash) -> Hash {
		return .Ordered([ annotation(extract), unwrap.hash(ifLeaf: leaf, ifRecur: { $0.hash(annotation: annotation, leaf: leaf) }) ])
	}
}


import Prelude
