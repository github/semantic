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

	public static let Leaf = Syntax.Leaf >>> Roll
	public static let Branch: [Term] -> Term = Syntax.Branch >>> Roll


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
				self = .Roll(.Branch(try a.map { try Term(JSON: $0) ?? die() }))
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
					self = .Branch([ .Leaf(name), .Branch(try substructure.map { try Term(JSON: $0) ?? die() }) ])

				case .Some("source.lang.swift.decl.enumelement"):
					fallthrough
				case
					.Some("source.lang.swift.decl.function.method.instance"),
					.Some("source.lang.swift.decl.function.free"):
					self = .Branch([ .Leaf(name), .Branch(try substructure.map { try Term(JSON: $0) ?? die() }) ])

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
			return .Text(n)
		case let .Branch(vs):
			return vs.map(Doc.init).stack().bracket("{", "}")
		}
	}
}

extension Syntax where Recur: Hashable {
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
