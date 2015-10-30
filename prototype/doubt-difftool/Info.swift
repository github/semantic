struct Info: Categorizable, CustomJSONConvertible, Equatable {
	init(range: Range<Int>, categories: Set<String>) {
		self.range = range
		self.categories = categories
	}

	init(range: Range<String.CharacterView.Index>, leaf: JSONLeaf) {
		// FIXME: this is terrible. see also https://github.com/github/semantic-diff/issues/136
		self.range = Int(String(range.startIndex))!..<Int(String(range.endIndex))!

		switch leaf {
		case .Number:
			self.categories = [ "number" ]
		case .Boolean:
			self.categories = [ "boolean" ]
		case .String:
			self.categories = [ "string" ]
		case .Null:
			self.categories = [ "null" ]
		}
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
