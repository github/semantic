public enum Term: CustomDebugStringConvertible, CustomDocConvertible, CustomStringConvertible, Equatable {
	public init(_ out: Syntax<Term>) {
		self = .Roll(out)
	}

	case Empty
	indirect case Roll(Syntax<Term>)

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Roll(s):
			return s.debugDescription
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Roll(s):
			return s.doc
		}
	}


	public static let Apply: (Term, [Term]) -> Term = Syntax.Apply >>> Roll
	public static let Abstract: ([Term], [Term]) -> Term = Syntax.Abstract >>> Roll
	public static let Assign: (String, Term) -> Term = Syntax.Assign >>> Roll
	public static let Variable = Syntax.Variable >>> Roll
	public static let Literal = Syntax.Literal >>> Roll
	public static let Group: (Term, [Term]) -> Term = Syntax.Group >>> Roll
}

public enum Syntax<Payload>: CustomDebugStringConvertible, CustomDocConvertible {
	case Apply(Payload, [Payload])
	case Abstract([Payload], [Payload])
	case Assign(String, Payload)
	case Variable(String)
	case Literal(String)
	case Group(Payload, [Payload])

	public func map<T>(@noescape transform: Payload -> T) -> Syntax<T> {
		switch self {
		case let .Apply(f, args):
			return .Apply(transform(f), args.map(transform))
		case let .Abstract(parameters, body):
			return .Abstract(parameters.map(transform), body.map(transform))
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

	public func reduce<T>(var initial: T, @noescape combine: (T, Payload) throws -> T) rethrows -> T {
		switch self {
		case let .Apply(x, xs):
			initial = try combine(initial, x)
			return try xs.reduce(initial, combine: combine)

		case let .Abstract(xs, x):
			initial = try xs.reduce(initial, combine: combine)
			return try x.reduce(initial, combine: combine)

		case let .Assign(_, x):
			return try combine(initial, x)

		case let .Group(x, xs):
			initial = try combine(initial, x)
			return try xs.reduce(initial, combine: combine)

		default:
			return initial
		}
	}

	public typealias Recur = Payload

	public var debugDescription: String {
		switch self {
		case let .Apply(f, vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Apply(\(f), [ \(s) ])"
		case let .Abstract(parameters, body):
			let s = parameters.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Abstract([ \(s) ], \(body))"
		case let .Assign(n, v):
			return ".Assign(\(n), \(v))"
		case let .Variable(n):
			return ".Variable(\(n))"
		case let .Literal(s):
			return ".Literal(\(s))"
		case let .Group(n, vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Group(\(String(reflecting: n)), [ \(s) ])"
		}
	}

	public var doc: Doc {
		switch self {
		case let .Apply(f, vs):
			return .Horizontal([
				Doc(f),
				.Wrap(.Text("("), .Join(.Text(", "), vs.map(Doc.init)), .Text(")"))
			])
		case let .Abstract(parameters, body):
			return .Horizontal([
				.Text("λ"),
				.Join(.Text(", "), parameters.map(Doc.init)),
				.Text("."),
				.Vertical(body.map(Doc.init))
			])
		case let .Assign(n, v):
			return .Horizontal([ .Text(n), .Text("="), Doc(v) ])
		case let .Variable(n):
			return .Text(n)
		case let .Literal(s):
			return .Text(s)
		case let .Group(n, vs):
			return .Horizontal([
				Doc(n),
				.Wrap(.Text("{"), .Vertical(vs.map(Doc.init)), .Text("}"))
			])
		}
	}
}
