public enum Doc: CustomDebugStringConvertible, CustomDocConvertible, Equatable {
	case Empty
	indirect case Concat(Doc, Doc)
	indirect case Union(Doc, Doc)
	case Text(String)
	indirect case Nest(Int, Doc)
	case Line

	public init<T>(_ value: T) {
		self = (value as? CustomDocConvertible)?.doc ?? .Text(String(value))
	}

	public func group() -> Doc {
		return Union(self.flattened, self)
	}

	public func bracket(left: String, _ right: String) -> Doc {
		return (Text(left)
			<> Nest(2, Line <> self)
			<> Line
			<> Text(right)).group()
	}

	public var flattened: Doc {
		switch self {
		case .Empty, .Text:
			return self
		case let .Concat(a, b):
			return .Concat(a.flattened, b.flattened)
		case let .Union(a, _):
			return a.flattened
		case let .Nest(i, doc):
			return .Nest(i, doc.flattened)
		case .Line:
			return .Text(" ")
		}
	}

	public func pretty(width: Int) -> String {
		return best(width).description
	}

	private func best(width: Int, placed: Int = 0) -> Layout {
		return Layout(width: width, placed: placed, alternatives: .pure((0, self)))
	}

	public var doc: Doc {
		return self
	}


	// MARK: CustomDebugStringConvertible

	public var debugDescription: String {
		switch self {
		case .Empty:
			return ".Empty"
		case let .Concat(a, b):
			return ".Concat(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Union(a, b):
			return ".Union(\(String(reflecting: a)), \(String(reflecting: b)))"
		case let .Text(s):
			return ".Text(\(String(reflecting: s)))"
		case let .Nest(i, x):
			return ".Nest(\(String(reflecting: i)), \(String(reflecting: x)))"
		case .Line:
			return ".Line"
		}
	}
}


public func == (left: Doc, right: Doc) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty), (.Line, .Line):
		return true
	case let (.Text(a), .Text(b)):
		return a == b
	case let (.Nest(i, a), .Nest(j, b)):
		return i == j && a == b
	case let (.Concat(l1, r1), .Concat(l2, r2)):
		return l1 == l2 && r1 == r2
	case let (.Union(l1, r1), .Union(l2, r2)):
		return l1 == l2 && r1 == r2
	default:
		return false
	}
}


extension SequenceType where Generator.Element == Doc {
	public func fold(combine: (Doc, Doc) -> Doc) -> Doc {
		func fold(docs: Stream<Doc>) -> Doc {
			switch docs {
			case .Nil:
				return .Empty
			case let .Cons(x, rest) where rest.value.isEmpty:
				return x
			case let .Cons(x, rest):
				return combine(x, fold(rest.value))
			}
		}
		return fold(Stream(sequence: self))
	}

	public func spread() -> Doc {
		return fold(<+>)
	}

	public func stack() -> Doc {
		return fold(</>)
	}

	public func joinWithSeparator(separator: String) -> Doc {
		return fold {
			$0 <> .Text(separator) </> $1
		}
	}
}

public protocol CustomDocConvertible: CustomStringConvertible {
	var doc: Doc { get }
}

extension CustomDocConvertible {
	public var description: String {
		return doc.pretty(70)
	}
}


public func <> (left: Doc, right: Doc) -> Doc {
	return .Concat(left, right)
}


public func <+> (left: Doc, right: Doc) -> Doc {
	return left <> .Text(" ") <> right
}

public func </> (left: Doc, right: Doc) -> Doc {
	return left <> .Line <> right
}


private enum Layout: CustomStringConvertible, Equatable {
	case Empty
	indirect case Text(String, Layout)
	indirect case Line(Int, Layout)

	init(width: Int, placed: Int, alternatives: Stream<(Int, Doc)>) {
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

	var description: String {
		switch self {
		case .Empty:
			return ""
		case let .Text(s, doc):
			return s + doc.description
		case let .Line(n, doc):
			return "\n" + String(count: n, repeatedValue: " " as Character) + doc.description
		}
	}

	func fits(width: Int) -> Bool {
		guard width >= 0 else { return false }
		switch self {
		case .Empty, .Line:
			return true
		case let .Text(s, x):
			return x.fits(width - Int(s.characters.count))
		}
	}

	static func better(width: Int, _ placed: Int, _ x: Layout, _ y: Layout) -> Layout {
		return x.fits(width - placed) ? x : y
	}
}

private func == (left: Layout, right: Layout) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Text(a, x), .Text(b, y)):
		return a == b && x == y
	case let (.Line(i, x), .Line(j, y)):
		return i == j && x == y
	default:
		return false
	}
}
