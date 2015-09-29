public enum Layout: CustomStringConvertible, Equatable {
	case Empty
	indirect case Text(String, Layout)
	indirect case Line(Int, Layout)

	public init(width: Int, placed: Int, alternatives: Stream<(Int, DOC)>) {
		switch alternatives {
		case .Nil:
			self = .Empty
		case let .Cons((_, .Empty), rest):
			self = Layout(width: width, placed: placed, alternatives: rest.value)
		case let .Cons((i, .Concat(x, y)), rest):
			self = Layout(width: width, placed: placed, alternatives: .Cons((i, x), Memo(evaluated: .Cons((i, y), rest))))
		case let .Cons((i, .Nest(j, x)), rest):
			self = Layout(width: width, placed: placed, alternatives: .Cons((i + j, x), rest))
		case let .Cons((_, .Text(s)), rest):
			self = .Text(s, Layout(width: width, placed: placed + Int(s.characters.count), alternatives: rest.value))
		case let .Cons((i, .Line), rest):
			self = .Line(i, Layout(width: width, placed: i, alternatives: rest.value))
		case let .Cons((i, .Union(x, y)), z):
			self = .better(width, placed, Layout(width: width, placed: placed, alternatives: .Cons((i, x), z)), Layout(width: width, placed: placed, alternatives: .Cons((i, y), z)))
		}
	}

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

	public func fits(width: Int) -> Bool {
		guard width >= 0 else { return false }
		switch self {
		case .Empty, .Line:
			return true
		case let .Text(s, x):
			return x.fits(width - Int(s.characters.count))
		}
	}

	public static func better(width: Int, _ placed: Int, _ x: Layout, _ y: Layout) -> Layout {
		return x.fits(width - placed) ? x : y
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
		return best(width).description
	}

	public func best(width: Int, placed: Int = 0) -> Layout {
		return Layout(width: width, placed: placed, alternatives: .pure((0, self)))
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
