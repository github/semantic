public enum Doc: CustomStringConvertible, Equatable {
	case Empty
	indirect case Text(String, Doc)
	indirect case Line(Int, Doc)

	public var description: String {
		switch self {
		case .Empty:
			return ""
		case let .Text(s, doc):
			return s + doc.description
		case let .Line(n, doc):
			return "\n" + String(count: n, repeatedValue: " " as Character) + doc.description
		}
	}
}

public enum DOC {
	case Empty
	indirect case Concat(DOC, DOC)
	indirect case Union(DOC, DOC)
	case Text(String)
	indirect case Nest(Int, DOC)
	case Line

	public init<T>(_ value: T) {
		self = (value as? CustomDocConvertible)?.doc ?? .Text(String(value))
	}

	public static func group(doc: DOC) -> DOC {
		return Union(doc.flattened, doc)
	}

	public static func bracket(l: String, _ x: DOC, _ r: String) -> DOC {
		return group(Concat(Text(l), Concat(Nest(2, Concat(Line, x)), Concat(Line, Text(r)))))
	}

	public static func folddoc<C: CollectionType where C.Generator.Element == DOC>(docs: C, combine: (DOC, DOC) -> DOC) -> DOC {
		func fold(docs: Stream<DOC>) -> DOC {
			switch docs {
			case .Nil:
				return .Empty
			case let .Cons(x, rest) where rest.value.isEmpty:
				return x
			case let .Cons(x, rest):
				return combine(x, fold(rest.value))
			}
		}
		return fold(Stream(sequence: docs))
	}

	public static func spread<C: CollectionType where C.Generator.Element == DOC>(docs: C) -> DOC {
		return folddoc(docs, combine: <+>)
	}

	public static func stack<C: CollectionType where C.Generator.Element == DOC>(docs: C) -> DOC {
		return folddoc(docs, combine: </>)
	}

	public static func join<C: CollectionType where C.Generator.Element == DOC>(separator: String, _ docs: C) -> DOC {
		return folddoc(docs) {
			$0 <> Text(separator) <+> $1
		}
	}

	public var flattened: DOC {
		switch self {
		case .Empty, .Text:
			return self
		case let .Concat(a, b):
			return .Concat(a.flattened, b.flattened)
		case let .Union(a, _):
			return a.flattened
		case let .Nest(_, doc):
			return doc.flattened
		case .Line:
			return .Text(" ")
		}
	}

	public func pretty(width: Int) -> String {
		return best(width, 0, self).description
	}
}

func best(w: Int, _ k: Int, _ x: DOC) -> Doc {
	return be(w, k, Stream<(Int, DOC)>.pure((0, x)))
}

func be(w: Int, _ k: Int, _ z: Stream<(Int, DOC)>) -> Doc {
	switch z {
	case .Nil:
		return .Empty
	case let .Cons((_, .Empty), z):
		return be(w, k, z.value)
	case let .Cons((i, .Concat(x, y)), z):
		return be(w, k, .Cons((i, x), Memo(evaluated: .Cons((i, y), z))))
	case let .Cons((i, .Nest(j, x)), z):
		return be(w, k, .Cons((i + j, x), z))
	case let .Cons((_, .Text(s)), z):
		return .Text(s, be(w, k + Int(s.characters.count), z.value))
	case let .Cons((i, .Line), z):
		return .Line(i, be(w, i, z.value))
	case let .Cons((i, .Union(x, y)), z):
		return better(w, k, be(w, k, .Cons((i, x), z)), be(w, k, .Cons((i, y), z)))
	}
}

func better(w: Int, _ k: Int, _ x: Doc, _ y: Doc) -> Doc {
	return fits(w - k, x)
		? x
		: y
}

func fits(w: Int, _ x: Doc) -> Bool {
	guard w >= 0 else { return false }
	switch x {
	case .Empty, .Line:
		return true
	case let .Text(s, x):
		return fits(w - Int(s.characters.count), x)
	}
}


public protocol CustomDocConvertible: CustomStringConvertible {
	var doc: DOC { get }
}

extension CustomDocConvertible {
	public var description: String {
		return doc.pretty(70)
	}
}


public func <> (left: DOC, right: DOC) -> DOC {
	return .Concat(left, right)
}


public func <+> (left: DOC, right: DOC) -> DOC {
	return left <> .Text(" ") <> right
}

public func </> (left: DOC, right: DOC) -> DOC {
	return left <> .Line <> right
}
