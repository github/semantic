struct State<T> {
	let rest: String
	let value: T
}

prefix func ^(string: String)(_ input: String) -> State<String>? {
	return input.characters.startsWith(string.characters)
		? State(rest: input.from(string.characters.count), value: string)
		: nil
}

prefix func ^(strings: [String])(_ input: String) -> State<String>? {
	return strings.indexOf { input.characters.startsWith($0.characters) }
		.flatMap { index in
			let string = strings[index]
			return (^string)(input)
		}
}

func parseWhile(predicate: Character -> Bool)(_ input: String) -> State<String>? {
	return input.characters.count > 0 && predicate(input.characters[input.startIndex])
		? parseWhile(predicate)(input.from(1)).map { State(rest: $0.rest, value: input.to(1) + $0.value) } ?? State(rest: input.from(1), value: input.to(1))
		: nil
}

postfix func * <T>(parser: String -> State<T>?) -> String -> State<[T]>? {
	return (Array.cons <^> (parser <*> { (parser*)($0) })) <|> pure([])
}

postfix func + <T> (parser: String -> State<T>?) -> String -> State<[T]>? {
	return (Array.cons <^> (parser <*> parser*))
}

func interpolate<Separator, Element>(element: String -> State<Element>?, _ separator: String -> State<Separator>?) -> String -> State<[Element]>? {
	return Array.cons <^> (element <*> (separator *> element)*) <|> pure([])
}

func <*> <T, U> (left: String -> State<T>?, right: String -> State<U>?)(_ input: String) -> State<(T, U)>? {
	return left(input).flatMap { l in
		right(l.rest).map { r in
			State(rest: r.rest, value: (l.value, r.value))
		}
	}
}

func <* <T, U> (left: String -> State<T>?, right: String -> State<U>?) -> String -> State<T>? {
	return left >>- { const($0) <^> right }
}

func *> <T, U> (left: String -> State<T>?, right: String -> State<U>?) -> String -> State<U>? {
	return left >>- const(right)
}

func <|> <T> (left: String -> State<T>?, right: String -> State<T>?)(_ input: String) -> State<T>? {
	return left(input) ?? right(input)
}

func <^> <T, U> (left: T -> U, right: String -> State<T>?) -> String -> State<U>? {
	return right >>- { pure(left($0)) }
}

func >>- <T, U> (left: String -> State<T>?, right: T -> String -> State<U>?)(_ input: String) -> State<U>? {
	return left(input).flatMap { state in right(state.value)(state.rest) }
}

func pure<A>(value: A)(_ input: String) -> State<A>? {
	return State(rest: input, value: value)
}
