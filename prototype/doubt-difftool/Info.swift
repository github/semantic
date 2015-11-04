struct Info: Categorizable, CustomJSONConvertible, Equatable {
	init(range: Range<Int>, line: Line, column: Column, categories: Set<String>) {
		self.range = range
		self.line = line
		self.column = column
		self.categories = categories
	}

	init(range: Range<String.CharacterView.Index>, line: Line, column: Column, categories: Set<String>) {
		// FIXME: this is terrible. see also https://github.com/github/semantic-diff/issues/136
		self.range = Int(String(range.startIndex))!..<Int(String(range.endIndex))!
		self.line = line
		self.column = column
		self.categories = categories
	}

	let range: Range<Int>

	let line: Line
	
	let column: Column


	// MARK: Categorizable

	let categories: Set<String>


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		return [
			"range": range.JSON,
			"line": line.JSON,
			"column": column.JSON,
			"categories": Array(categories).JSON
		]
	}
}

func == (left: Info, right: Info) -> Bool {
	return left.range == right.range && left.categories == right.categories
}

import Madness
import Doubt
