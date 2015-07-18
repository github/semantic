extension String {
	func from(offset: String.Index.Distance) -> String {
		return String(characters[advance(startIndex, offset, endIndex)..<endIndex])
	}

	func to(offset: String.Index.Distance) -> String {
		return String(characters[startIndex..<advance(startIndex, offset, endIndex)])
	}
}
