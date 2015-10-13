extension RangeReplaceableCollectionType {
	static func cons(head: Generator.Element, _ tail: Self) -> Self {
		return [ head ] + tail
	}
}
