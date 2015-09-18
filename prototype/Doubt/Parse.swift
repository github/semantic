public struct State<T> {
	public let rest: String
	public let value: T
}

public prefix func ^(string: String)(_ input: String) -> State<String>? {
	return input.characters.startsWith(string.characters)
		? State(rest: input.from(string.characters.count), value: string)
		: nil
}

public prefix func ^(strings: [String])(_ input: String) -> State<String>? {
	return strings.indexOf { input.characters.startsWith($0.characters) }
		.flatMap { index in
			let string = strings[index]
			return (^string)(input)
		}
}

public prefix func ^<S where S: SequenceType, S.Generator.Element == Character>(strings: S) -> String -> State<String>? {
	return ^strings.map { String($0) }
}


/// Parse the full string with parser or fail.
public func full<A>(parser: String -> State<A>?)(_ input: String) -> A? {
	return parser(input).flatMap { $0.rest.isEmpty ? $0.value : nil }
}


/// A convenience for use while developing parsers.
public func never<T>(_: String) -> State<T>? {
	return nil
}

//// Matches a single character that is not matched by `parser`.
public func not<T>(parser: String -> State<T>?)(_ input: String) -> State<String>? {
	if input.isEmpty { return nil }
	return parser(input).map(const(nil)) ?? State(rest: input.from(1), value: input.to(1))
}


/// Delays evaluation of a parser.
public func delay<A>(thunk: () -> String -> State<A>?) -> String -> State<A>? {
	return { thunk()($0) }
}


public func parseWhile(predicate: Character -> Bool)(_ input: String) -> State<String>? {
	return input.characters.count > 0 && predicate(input.characters[input.startIndex])
		? parseWhile(predicate)(input.from(1)).map { State(rest: $0.rest, value: input.to(1) + $0.value) } ?? State(rest: input.from(1), value: input.to(1))
		: nil
}

public postfix func * <T>(parser: String -> State<T>?) -> String -> State<[T]>? {
	return (Array.cons <^> (parser <*> { (parser*)($0) })) <|> pure([])
}

public postfix func + <T> (parser: String -> State<T>?) -> String -> State<[T]>? {
	return (Array.cons <^> (parser <*> parser*))
}

public func interpolate<Separator, Element>(element: String -> State<Element>?, _ separator: String -> State<Separator>?) -> String -> State<[Element]>? {
	return Array.cons <^> (element <*> (separator *> element)*) <|> pure([])
}

public func <*> <T, U> (left: String -> State<T>?, right: String -> State<U>?)(_ input: String) -> State<(T, U)>? {
	return left(input).flatMap { l in
		right(l.rest).map { r in
			State(rest: r.rest, value: (l.value, r.value))
		}
	}
}

public func <* <T, U> (left: String -> State<T>?, right: String -> State<U>?) -> String -> State<T>? {
	return left >>- { const($0) <^> right }
}

public func *> <T, U> (left: String -> State<T>?, right: String -> State<U>?) -> String -> State<U>? {
	return left >>- const(right)
}

public func <|> <T> (left: String -> State<T>?, right: String -> State<T>?)(_ input: String) -> State<T>? {
	return left(input) ?? right(input)
}

public func <^> <T, U> (left: T -> U, right: String -> State<T>?) -> String -> State<U>? {
	return right >>- { pure(left($0)) }
}

public func >>- <T, U> (left: String -> State<T>?, right: T -> String -> State<U>?)(_ input: String) -> State<U>? {
	return left(input).flatMap { state in right(state.value)(state.rest) }
}

public func pure<A>(value: A)(_ input: String) -> State<A>? {
	return State(rest: input, value: value)
}
