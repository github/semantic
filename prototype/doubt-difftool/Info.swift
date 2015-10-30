struct Info: Categorizable, CustomJSONConvertible, Equatable {
	init(range: Range<Int>, categories: Set<String>) {
		self.range = range
		self.categories = categories
	}

	let range: Range<Int>


	// MARK: Categorizable

	let categories: Set<String>


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		return [
			"range": range.JSON,
			"categories": Array(categories).JSON
		]
	}
}

func == (left: Info, right: Info) -> Bool {
	return left.range == right.range && left.categories == right.categories
}


import Doubt
