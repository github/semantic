public enum Term: CustomDebugStringConvertible, CustomDocConvertible, CustomStringConvertible, AlgebraicHashable {
	public init(_ out: Syntax<Term, String>) {
		self = .Roll(out)
	}

	indirect case Roll(Syntax<Term, String>)

	public var syntax: Syntax<Term, String> {
		switch self {
		case let .Roll(syntax):
			return syntax
		}
	}

	public var debugDescription: String {
		switch self {
		case let .Roll(s):
			return s.debugDescription
		}
	}

	public var doc: Doc {
		switch self {
		case let .Roll(s):
			return s.doc
		}
	}


	public var hash: Hash {
		return syntax.hash
	}


	public static var Empty: Term {
		return Term(.Empty)
	}

	public static let Apply: (Term, [Term]) -> Term = Syntax.Apply >>> Roll
	public static let Abstract: ([Term], [Term]) -> Term = Syntax.Abstract >>> Roll
	public static let Assign: (String, Term) -> Term = Syntax.Assign >>> Roll
	public static let Leaf = Syntax.Leaf >>> Roll
	public static let Group: (Term, [Term]) -> Term = Syntax.Group >>> Roll


	// MARK: JSON representation.

	/// Constructs a Term representing the `JSON` in a file at `path`.
	public init?(path: String, JSON: Doubt.JSON) {
		struct E: ErrorType {}
		func die<A>() throws -> A {
			throw E()
		}
		do {
			switch JSON.dictionary?["key.substructure"] {
			case let .Some(.Array(a)):
				self = .Roll(.Group(.Roll(.Leaf(path)), try a.map { try Term(JSON: $0) ?? die() }))
			default:
				return nil
			}
		} catch _ {
			return nil
		}
	}

	/// Constructs a Term representing `JSON`.
	public init?(JSON: Doubt.JSON) {
		enum Key: String {
			case Name = "key.name"
			case Substructure = "key.substructure"
		}
		struct E: ErrorType {}
		func die<A>() throws -> A {
			throw E()
		}
		do {
			switch JSON {
			case let .Dictionary(d) where d["key.name"] != nil:
				let name = d["key.name"]?.string ?? ""
				let substructure = d["key.substructure"]?.array ?? []
				let kind = d["key.kind"]?.string
				switch kind {
				case
					.Some("source.lang.swift.decl.class"),
					.Some("source.lang.swift.decl.extension"),
					.Some("source.lang.swift.decl.enum"),
					.Some("source.lang.swift.decl.struct"):
					self = .Group(.Leaf(name), try substructure.map { try Term(JSON: $0) ?? die() })

				case .Some("source.lang.swift.decl.enumelement"):
					fallthrough
				case
					.Some("source.lang.swift.decl.function.method.instance"),
					.Some("source.lang.swift.decl.function.free"):
					self = .Assign(name, .Abstract([], try substructure.map { try Term(JSON: $0) ?? die() }))

				case
					.Some("source.lang.swift.decl.var.instance"),
					.Some("source.lang.swift.decl.var.static"):
					self = .Leaf(name)

				default:
					return nil
				}

			case let .Dictionary(d) where d["key.kind"]?.string == "source.lang.swift.decl.enumcase" && d["key.substructure"]?.array?.count == 1:
				let substructure = d["key.substructure"]?.array ?? []
				self = try Term(JSON: substructure[0]) ?? die()

			case let .Dictionary(d) where d["key.kind"]?.string == "source.lang.swift.syntaxtype.comment.mark":
				self = .Empty

			case .Null:
				self = .Empty

			default:
				return nil
			}
		} catch _ {
			return nil
		}
	}
}


public enum Syntax<Recur, A>: CustomDebugStringConvertible, CustomDocConvertible {
	case Empty
	case Leaf(String)
	case Apply(Recur, [Recur])
	case Abstract([Recur], [Recur])
	case Assign(String, Recur)
	case Group(Recur, [Recur])

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case .Empty:
			return .Empty
		case let .Apply(f, args):
			return .Apply(transform(f), args.map(transform))
		case let .Abstract(parameters, body):
			return .Abstract(parameters.map(transform), body.map(transform))
		case let .Assign(n, v):
			return .Assign(n, transform(v))
		case let .Leaf(n):
			return .Leaf(n)
		case let .Group(n, v):
			return .Group(transform(n), v.map(transform))
		}
	}

	public func reduce<T>(var initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
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

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Apply(f, vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Apply(\(f), [ \(s) ])"
		case let .Abstract(parameters, body):
			let s = parameters.map { String(reflecting: $0) }.joinWithSeparator(", ")
			let b = body.map { String(reflecting: $0) }.joinWithSeparator("\n")
			return ".Abstract([ \(s) ], \(b))"
		case let .Assign(n, v):
			return ".Assign(\(n), \(v))"
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Group(n, vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Group(\(String(reflecting: n)), [ \(s) ])"
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Apply(f, vs):
			return Doc(f) <> vs.map(Doc.init).joinWithSeparator(",").bracket("(", ")")
		case let .Abstract(parameters, body):
			return .Text("λ")
				<> parameters.map(Doc.init).joinWithSeparator(",")
				<> .Text(".")
				<> body.map(Doc.init).stack()
		case let .Assign(n, v):
			return .Text(n) <+> .Text("=") <+> Doc(v)
		case let .Leaf(n):
			return .Text(n)
		case let .Group(n, vs):
			return Doc(n) <> vs.map(Doc.init).stack().bracket("{", "}")
		}
	}
}

extension Syntax where Recur: AlgebraicHashable {
	public var hash: Hash {
		switch self {
		case .Empty:
			return .Case("Empty")
		case let .Apply(f, vs):
			return .Case("Apply", .Sequence([ f.hash ] + vs.map { $0.hash }))
		case let .Abstract(parameters, body):
			return .Case("Abstract", .Sequence(parameters.map { $0.hash }), .Sequence(body.map { $0.hash }))
		case let .Assign(name, value):
			return .Case("Assign", .String(name), value.hash)
		case let .Leaf(n):
			return .Case("Leaf", .String(n))
		case let .Group(n, vs):
			return .Case("Group", n.hash, .Sequence(vs.map { $0.hash }))
		}
	}
}
