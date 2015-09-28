struct BoundsCheckedArray<Element>: CollectionType {
	init(array: [Element]) {
		self.array = array
	}

	let array: [Element]

	let startIndex = 0
	var endIndex: Int {
		return array.count
	}

	subscript (i: Int) -> Element? {
		return i < count ? array[i] : nil
	}
}
