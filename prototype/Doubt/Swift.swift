enum Swift: Equatable {
	case KeyValue(String, String)
	case Branch(String, [Swift])

	struct Parsers {
		static let alphabetic = ^"abcdefghijklmnopqrstuvwxyz".characters
		static let ws = ^" \t\n".characters

		static let word = concat <^> (alphabetic <|> ^"_")+
		static let quoted = join(^"'", join((concat <^> not(^"'")*), ^"'"))

		static let keyValue = KeyValue <^> (word <* ^"=" <*> (quoted <|> (concat <^> not(ws <|> ^")")*)))
		static let branch = Branch <^> (^"(" *> ws* *> word <*> sexpr* <* ws* <* ^")")
		static let sexpr: String -> State<Swift>? = never <* ws*
	}
}

private func join(a: String -> State<String>?, _ b: String -> State<String>?) -> String -> State<String>? {
	return (+) <^> (a <*> b)
}

private func concat(strings: [String]) -> String {
	return strings.joinWithSeparator("")
}
