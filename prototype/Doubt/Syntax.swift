public enum Term<A>: CustomDebugStringConvertible, CustomDocConvertible, CustomStringConvertible {
	public init(_ out: Syntax<Term, A>) {
		self = .Roll(out)
	}

	indirect case Roll(Syntax<Term, A>)

	public var syntax: Syntax<Term, A> {
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


	public static var Empty: Term {
		return Term(Syntax<Term, A>.Empty)
	}

	public static func Leaf(a: A) -> Term {
		return Roll(.Leaf(a))
	}

	public static func Branch(terms: [Term]) -> Term {
		return Roll(.Branch(terms))
	}
}

public protocol StringConvertible {
	init(string: String)
}

extension String: StringConvertible {
	public init(string: String) {
		self = string
	}
}

extension Term where A: StringConvertible {
	/// Constructs a Term representing `JSON`.
	public init?(JSON: Doubt.JSON) {
		func die<B>() throws -> B {
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
					self = .Branch([ .Leaf(A(string: name)), .Branch(try substructure.map { try Term(JSON: $0) ?? die() }) ])

				case .Some("source.lang.swift.decl.enumelement"):
					fallthrough
				case
					.Some("source.lang.swift.decl.function.method.instance"),
					.Some("source.lang.swift.decl.function.free"):
					self = .Branch([ .Leaf(A(string: name)), .Branch(try substructure.map { try Term(JSON: $0) ?? die() }) ])

				case
					.Some("source.lang.swift.decl.var.instance"),
					.Some("source.lang.swift.decl.var.static"):
					self = .Leaf(A(string: name))

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

	/// Constructs a Term representing the `JSON` in a file at `path`.
	public init?(path: String, JSON: Doubt.JSON) {
		func die<B>() throws -> B {
			throw E()
		}
		do {
			switch JSON.dictionary?["key.substructure"] {
			case let .Some(.Array(a)):
				self = .Roll(.Branch(try a.map { try Term(JSON: $0) ?? die() }))
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
	case Leaf(A)
	case Branch([Recur])

	public func map<T>(@noescape transform: Recur -> T) -> Syntax<T, A> {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return .Leaf(n)
		case let .Branch(vs):
			return .Branch(vs.map(transform))
		}
	}

	public func reduce<T>(initial: T, @noescape combine: (T, Recur) throws -> T) rethrows -> T {
		switch self {
		case let .Branch(xs):
			return try xs.reduce(initial, combine: combine)

		default:
			return initial
		}
	}

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Leaf(n):
			return ".Leaf(\(n))"
		case let .Branch(vs):
			let s = vs.map { String(reflecting: $0) }.joinWithSeparator(", ")
			return ".Branch([ \(s) ])"
		}
	}

	public var doc: Doc {
		switch self {
		case .Empty:
			return .Empty
		case let .Leaf(n):
			return Doc(n)
		case let .Branch(vs):
			return vs.map(Doc.init).stack().bracket("{", "}")
		}
	}
}

extension Syntax where Recur: Hashable, A: Hashable {
	public var hash: Hash {
		switch self {
		case .Empty:
			return Hash("Empty")
		case let .Leaf(n):
			return Hash("Leaf", Hash(n))
		case let .Branch(vs):
			return Hash("Branch", .Sequence(vs.map(Hash.init)))
		}
	}
}

private struct E: ErrorType {}
