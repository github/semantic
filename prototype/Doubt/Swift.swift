enum Swift: Equatable {
	case KeyValue(String, String)
	case Branch(String, [Swift])

	struct Parsers {
		static let alphabetic = ^"abcdefghijklmnopqrstuvwxyz".characters
		static let word = { $0.joinWithSeparator("") } <^> alphabetic*
		static let ws = ^" \t\n".characters
		static let keyValue = KeyValue <^> (word <* ^"=" <*> word)
		static let branch = Branch <^> (^"(" *> ws* *> word <*> sexpr* <* ws* <* ^")")
		static let sexpr: String -> State<Swift>? = never <* ws*
	}
}
