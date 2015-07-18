let word = ^("abcdefghijklmnopqrstuvwxyz".characters.map { String($0) })
let ws = ^" "

let term: String -> State<Fix>? = fix { term in
	let variable = Syntax<Fix>.Variable <^> word
	let application = Syntax.Apply <^> (^"(" *> ws* *> term <*> (ws *> term)* <* ws* <* ^")")
	let abstraction = Syntax.Abstract <^> (^"λ" *> ws* *> interpolate(term, ws) <*> (ws* *> ^"." *> term))
	return Fix.init <^> (abstraction <|> application <|> variable)
}
