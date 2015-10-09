public struct BoundsCheckedArray<Element>: CollectionType {
	public init(array: [Element]) {
		self.array = array
	}

	let array: [Element]

	public let startIndex = 0
	public var endIndex: Int {
		return array.count
	}

	public subscript (i: Int) -> Element? {
		return i < count ? array[i] : nil
	}
}
