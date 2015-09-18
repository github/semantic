let atom = Syntax<Term>.Variable <^> ^("abcdefghijklmnopqrstuvwxyz".characters.map { String($0) })
let ws = ^" \t\n".characters

let sexpr: String -> State<Term>? = fix { sexpr in
	let list = Syntax<Term>.Apply <^> (ws* *> ^"(" *> ws* *> sexpr <*> sexpr* <* ^")")
	return Term.init <^> (atom <|> list) <* ws*
}
