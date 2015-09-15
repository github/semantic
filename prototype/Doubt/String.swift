extension String {
	func from(offset: String.Index.Distance) -> String {
		return String(characters[startIndex.advancedBy(offset, limit: endIndex)..<endIndex])
	}

	func to(offset: String.Index.Distance) -> String {
		return String(characters[startIndex..<startIndex.advancedBy(offset, limit: endIndex)])
	}
}
