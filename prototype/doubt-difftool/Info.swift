struct Info: Categorizable, CustomJSONConvertible, Equatable {
	init(range: Range<Int>, lines: Range<Line>, columns: Range<Column>, categories: Set<String>) {
		self.range = range
		self.lines = lines
		self.columns = columns
		self.categories = categories
	}

	init(range: Range<String.CharacterView.Index>, lines: Range<Line>, columns: Range<Column>, categories: Set<String>) {
		// FIXME: this is terrible. see also https://github.com/github/semantic-diff/issues/136
		self.range = Int(String(range.startIndex))!..<Int(String(range.endIndex))!
		self.lines = lines
		self.columns = columns
		self.categories = categories
	}

	let range: Range<Int>

	let lines: Range<Line>
	
	let columns: Range<Column>


	// MARK: Categorizable

	let categories: Set<String>


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		return [
			"range": range.JSON,
			"lines": lines.JSON,
			"columns": columns.JSON,
			"categories": Array(categories).JSON
		]
	}
}

func == (left: Info, right: Info) -> Bool {
	return left.range == right.range && left.categories == right.categories && left.lines == left.lines && left.columns == right.columns
}

import Madness
import Doubt
