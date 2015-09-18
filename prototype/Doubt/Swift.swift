let alphabetic = ^"abcdefghijklmnopqrstuvwxyz".characters
let word = { $0.joinWithSeparator("") } <^> alphabetic*
let ws = ^" \t\n".characters

func never<T>(_: String) -> State<T>? {
	return nil
}

enum Swift {
	case KeyValue(String, String)
	case Branch(String, [Swift])

	struct Parsers {
		static let keyValue = KeyValue <^> (word <* ^"=" <*> word)
		static let branch = Branch <^> (^"(" *> ws* *> word <*> sexpr* <* ws* <* ^")")
		static let sexpr: String -> State<Swift>? = never <* ws*
	}
}
