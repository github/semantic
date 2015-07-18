extension Array {
	static func cons(head: Element, _ tail: Array) -> Array {
		return [head] + tail
	}
}
