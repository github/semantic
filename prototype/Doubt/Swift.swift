enum Swift: Equatable {
	case Atom(String)
	case Symbol(String, [String])
	case KeyValue(String, String)
	case Branch(String, [Swift])

	struct Parsers {
		static let alphabetic = ^"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".characters
		static let ws = ^" \t\n".characters

		static let word = concat <^> (alphabetic <|> ^"_")+
		static let atom = concat <^> not(ws <|> ^")")*
		static let quoted = join(^"'", join((concat <^> not(^"'")*), ^"'"))

		static let symbol = Swift.Symbol <^> (join(^"\"", join((concat <^> not(^"\"")*), ^"\"")) <*> (^"<" *> interpolate(concat <^> alphabetic+, ^"," <* ws*) <* ^">"))
		static let keyValue = KeyValue <^> (word <* ^"=" <*> (quoted <|> atom))
		static let branch: String -> State<Swift>? = Branch <^> (^"(" *> ws* *> word <* ws* <*> sexpr* <* ws* <* ^")")
		static let sexpr = delay { (branch <|> symbol <|> (Swift.Atom <^> atom) <|> keyValue) <* ws* }
	}
}

private func join(a: String -> State<String>?, _ b: String -> State<String>?) -> String -> State<String>? {
	return (+) <^> (a <*> b)
}

private func concat(strings: [String]) -> String {
	return strings.joinWithSeparator("")
}
