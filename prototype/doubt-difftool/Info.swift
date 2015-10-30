struct Info: Categorizable, CustomJSONConvertible, Equatable {
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
