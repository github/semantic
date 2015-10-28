/// The cofree comonad over `Syntax`.
///
/// This is “free” in the sense of “unconstrained” rather than “zero-cost”; it’s the comonad obtained by taking a functor (in this case `Syntax`) and adding the minimum necessary details (the `Annotation` paired with it) to satisfy the comonad laws.
///
/// This type is dual to `Free`. Where `Free` is inhabited by syntax trees where some terms are replaced with `Annotation`s, `Cofree` is inhabited by syntax trees where all terms are annotated with `Annotation`s. In Doubt, this allows us to e.g. annotate terms with source range information, categorization, etc.
public enum Cofree<Leaf, Annotation> {
	indirect case Unroll(Annotation, Syntax<Cofree, Leaf>)

	public var unwrap: Syntax<Cofree, Leaf> {
		switch self {
		case let .Unroll(_, rest):
			return rest
		}
	}


	public init(_ annotation: Annotation, _ syntax: Syntax<Cofree, Leaf>) {
		self = .Unroll(annotation, syntax)
	}
}


// MARK: - CustomDebugStringConvertible

extension Cofree: CustomDebugStringConvertible {
	public var debugDescription: String {
		return "(\(String(reflecting: extract)), \(String(reflecting: unwrap)))"
	}
}


// MARK: - Functor

extension Cofree {
	public func map<Other>(@noescape transform: Annotation throws -> Other) rethrows -> Cofree<Leaf, Other> {
		return try .Unroll(transform(extract), unwrap.map { try $0.map(transform) })
	}
}


// MARK: - Comonad

extension Cofree {
	/// Returns the value annotating the syntax tree at this node.
	public var extract: Annotation {
		switch self {
		case let .Unroll(b, _):
			return b
		}
	}

	/// Returns a new `Cofree` by recursively applying `transform` to each node, producing the annotations for the copy.
	public func extend<Other>(transform: Cofree -> Other) -> Cofree<Leaf, Other> {
		return .Unroll(transform(self), unwrap.map { $0.extend(transform) })
	}

	/// Returns a new `Cofree` constructed by recursively annotating each subtree with itself.
	public var duplicate: Cofree<Leaf, Cofree<Leaf, Annotation>> {
		return extend(id)
	}
}


// MARK: - Equality

extension Cofree {
	public static func equals(annotation annotation: (Annotation, Annotation) -> Bool, leaf: (Leaf, Leaf) -> Bool)(_ left: Cofree, _ right: Cofree) -> Bool {
		return annotation(left.extract, right.extract)
			&& Syntax.equals(leaf: leaf, recur: Cofree.equals(annotation: annotation, leaf: leaf))(left.unwrap, right.unwrap)
	}
}

public func == <Leaf: Equatable, Annotation: Equatable> (left: Cofree<Leaf, Annotation>, right: Cofree<Leaf, Annotation>) -> Bool {
	return Cofree.equals(annotation: ==, leaf: ==)(left, right)
}


// MARK: - JSON

extension Cofree {
	public func JSON(annotation annotation: Annotation -> Doubt.JSON, leaf: Leaf -> Doubt.JSON) -> Doubt.JSON {
		return [
			"extract": annotation(extract),
			"unwrap": unwrap.JSON(leaf: leaf, recur: { $0.JSON(annotation: annotation, leaf: leaf) })
		]
	}
}

extension Cofree where Leaf: CustomJSONConvertible, Annotation: CustomJSONConvertible {
	public var JSON: Doubt.JSON {
		return JSON(annotation: { $0.JSON }, leaf: { $0.JSON })
	}
}


// MARK: - Categorizable

extension Cofree where Annotation: Categorizable {
	var categories: Set<Annotation.Category> {
		return extract.categories
	}
}


// MARK: - CofreeType

public protocol CofreeType: TermType {
	typealias Annotation

	init(_ annotation: Annotation, _ syntax: Syntax<Self, Leaf>)
	var extract: Annotation { get }
}

extension CofreeType {
	public static func Introduce(annotation: Annotation)(syntax: Syntax<Self, Leaf>) -> Self {
		return Self(annotation, syntax)
	}

	public static func eliminate(term: Self) -> (Annotation, Syntax<Self, Leaf>) {
		return (term.extract, term.unwrap)
	}

	/// Constructs a cofree by coiteration.
	///
	/// This is an _anamorphism_ (from the Greek “ana,” “upwards”; compare “anabolism”), a generalization of unfolds over regular trees (and datatypes isomorphic to them). The initial seed is used as the annotation of the returned value. The continuation of the structure is unpacked by applying `annotate` to the seed and mapping the resulting syntax’s values recursively. In this manner, the structure is unfolded bottom-up, starting with `seed` and ending at the leaves.
	///
	/// As this is the dual of `cata`, it’s unsurprising that we have a similar guarantee: coiteration is linear in the size of the constructed tree.
	public static func ana(unfold: Annotation -> Syntax<Annotation, Leaf>)(_ seed: Annotation) -> Self {
		return Self(seed, unfold(seed).map(ana(unfold)))
	}

	/// `Zip` two `CofreeType` values into a single `Cofree`, pairing their annotations.
	///
	/// This is partial, returning `nil` for any pair of values which are not of the same “shape,” i.e. where they wrap `Syntax` values of different constructors. The values of leaves are always taken from the second parameter.
	public static func zip(a: Self, _ b: Self) -> Cofree<Leaf, (Annotation, Annotation)>? {
		let annotations = (a.extract, b.extract)
		switch (a.unwrap, b.unwrap) {
		case let (.Leaf, .Leaf(b)):
			return Cofree(annotations, .Leaf(b))
		case let (.Indexed(a), .Indexed(b)):
			return Cofree(annotations, .Indexed(Swift.zip(a, b).flatMap(zip)))
		case let (.Fixed(a), .Fixed(b)):
			return Cofree(annotations, .Fixed(Swift.zip(a, b).flatMap(zip)))
		case let (.Keyed(a), .Keyed(b)):
			return Cofree(annotations, .Keyed(Dictionary(elements: b.keys.flatMap { key in zip(a[key]!, b[key]!).map { (key, $0) } })))
		default:
			return nil
		}
	}
}

extension Cofree: CofreeType {}

extension CofreeType where Self.Annotation == Range<String.Index> {
	public func JSON(source: String) -> Doubt.JSON {
		return unwrap.JSON(
			leaf: { _ in .String(source[extract]) },
			recur: {
				[
					"range": [
						"offset": .Number(Double(source.startIndex.distanceTo($0.extract.startIndex))),
						"length": .Number(Double($0.extract.count)),
					],
					"unwrap": $0.JSON(source)
				]
		})
	}
}


import Prelude
