/// A type whose values belong to a set of categories.
public protocol Categorizable {
	typealias Category: Hashable

	var categories: Set<Category> { get }
}
