import Doubt
import SourceKittenFramework

let arguments = BoundsCheckedArray(array: Process.arguments)

public protocol StringConvertible {
	init(string: String)
}

extension String: StringConvertible {
	public init(string: String) {
		self = string
	}
}

private struct Bail: ErrorType {}

extension Term where A: StringConvertible {
	/// Constructs a Term representing `JSON`.
	init?(JSON: Doubt.JSON) {
		func bail<B>() throws -> B {
			throw Bail()
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
					self = .Branch([ .Leaf(A(string: name)), .Branch(try substructure.map { try Term(JSON: $0) ?? bail() }) ])

				case .Some("source.lang.swift.decl.enumelement"):
					fallthrough
				case
				.Some("source.lang.swift.decl.function.method.instance"),
				.Some("source.lang.swift.decl.function.free"):
					self = .Branch([ .Leaf(A(string: name)), .Branch(try substructure.map { try Term(JSON: $0) ?? bail() }) ])

				case
				.Some("source.lang.swift.decl.var.instance"),
				.Some("source.lang.swift.decl.var.static"):
					self = .Leaf(A(string: name))

				default:
					return nil
				}

			case let .Dictionary(d) where d["key.kind"]?.string == "source.lang.swift.decl.enumcase" && d["key.substructure"]?.array?.count == 1:
				let substructure = d["key.substructure"]?.array ?? []
				self = try Term(JSON: substructure[0]) ?? bail()

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
	init?(path: String, JSON: Doubt.JSON) {
		func bail<B>() throws -> B {
			throw Bail()
		}
		do {
			switch JSON.dictionary?["key.substructure"] {
			case let .Some(.Array(a)):
				self = .Roll(.Branch(try a.map { try Term(JSON: $0) ?? bail() }))
			default:
				return nil
			}
		} catch _ {
			return nil
		}
	}

	init?(path: String) {
		guard path != "/dev/null" else {
			self = .Empty
			return
		}
		guard let term = File(path: path)
			.map(Structure.init)
			.map({ $0.dictionary })
			.map(toAnyObject)
			.flatMap({ JSON(object: $0).flatMap { Term(path: path, JSON: $0) } }) else { return nil }
		self = term
	}
}

if let a = arguments[1].flatMap(Term<Info>.init), b = arguments[2].flatMap(Term<Info>.init) {
	print(String(reflecting: Diff(a, b)))
}
