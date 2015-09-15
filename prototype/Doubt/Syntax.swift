public struct Fix: CustomDebugStringConvertible, CustomDocConvertible, CustomStringConvertible, Equatable {
	public init(_ roll: () -> Syntax<Fix>) {
		self.roll = roll
	}
	public init(_ out: Syntax<Fix>) {
		self.init { out }
	}

	let roll: () -> Syntax<Fix>

	public var out: Syntax<Fix> {
		return roll()
	}

	public var debugDescription: String {
		return cata { String(reflecting: $0) } (self)
	}

	public var doc: Doc<Pretty> {
		return cata { (syntax: Syntax<Doc<Pretty>>) in syntax.doc } (self)
	}

	public var description: String {
		return cata { String($0) } (self)
	}
}

public enum Syntax<Payload>: CustomDebugStringConvertible, CustomDocConvertible {
	case Apply(Payload, [Payload])
	case Abstract([Payload], Payload)
	case Assign(String, Payload)
	case Variable(String)
	case Literal(String)
	case Group(Payload, [Payload])

	public func map<T>(@noescape transform: Payload -> T) -> Syntax<T> {
		switch self {
		case let .Apply(f, args):
			return .Apply(transform(f), args.map(transform))
		case let .Abstract(parameters, body):
			return .Abstract(parameters.map(transform), transform(body))
		case let .Assign(n, v):
			return .Assign(n, transform(v))
		case let .Variable(n):
			return .Variable(n)
		case let .Literal(v):
			return .Literal(v)
		case let .Group(n, v):
			return .Group(transform(n), v.map(transform))
		}
	}

	public typealias Recur = Payload

	public var debugDescription: String {
		switch self {
		case let .Apply(f, vs):
			let s = vs.map { String($0) }.joinWithSeparator(", ")
			return ".Apply(\(f), [ \(s) ])"
		case let .Abstract(parameters, body):
			let s = parameters.map { String($0) }.joinWithSeparator(", ")
			return ".Abstract([ \(s) ], \(body))"
		case let .Assign(n, v):
			return ".Assign(\(n), \(v))"
		case let .Variable(n):
			return ".Variable(\(n))"
		case let .Literal(s):
			return ".Literal(\(s))"
		case let .Group(n, vs):
			let s = vs.map { String($0) }.joinWithSeparator(", ")
			return ".Group(\(n), [ \(s) ])"
		}
	}

	public var doc: Doc<Pretty> {
		switch self {
		case let .Apply(f, vs):
			return .Horizontal([
				Pretty(f),
				Pretty.Wrap(Pretty.Text("("), Pretty.Join(Pretty.Text(", "), vs.map(Pretty.init)), Pretty.Text(")"))
			])
		case let .Abstract(parameters, body):
			return .Horizontal([
				Pretty.Text("λ"),
				Pretty.Join(Pretty.Text(", "), parameters.map(Pretty.init)),
				Pretty.Text("."),
				Pretty(body)
			])
		case let .Assign(n, v):
			return .Horizontal([ .Text(n), .Text("="), Pretty(v) ])
		case let .Variable(n):
			return .Text(n)
		case let .Literal(s):
			return .Text(s)
		case let .Group(n, vs):
			return .Horizontal([
				Pretty(n),
				Pretty.Wrap(.Text("{"), Pretty.Vertical(vs.map(Pretty.init)), .Text("}"))
			])
		}
	}
}


func cata<T>(f: Syntax<T> -> T)(_ term: Fix) -> T {
	return ({ $0.out } >>> { $0.map(cata(f)) } >>> f)(term)
}
