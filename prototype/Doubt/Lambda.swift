let word = ^("abcdefghijklmnopqrstuvwxyz".characters.map { String($0) })
let ws = ^" "

let term: String -> State<Term>? = fix { term in
	let variable = Syntax<Term>.Variable <^> word
	let application = Syntax.Apply <^> (^"(" *> ws* *> term <*> (ws *> term)* <* ws* <* ^")")
	let abstraction = Syntax.Abstract <^> (^"λ" *> ws* *> interpolate(term, ws) <*> (ws* *> ^"." *> term))
	return Term.init <^> (abstraction <|> application <|> variable)
}
