enum Doc<Payload>: AlgebraicType, CustomStringConvertible {
	case Empty
	case Text(String)
	case Horizontal([Payload])
	case Vertical([Payload])
	case Wrap(Payload, Payload, Payload)
	case Join(Payload, [Payload])

	func map<Other>(@noescape transform: Payload -> Other) -> Doc<Other> {
		switch self {
		case .Empty:
			return .Empty
		case let .Text(s):
			return .Text(s)
		case let .Horizontal(a):
			return .Horizontal(a.map(transform))
		case let .Vertical(a):
			return .Vertical(a.map(transform))
		case let .Wrap(open, body, close):
			return .Wrap(transform(open), transform(body), transform(close))
		case let .Join(separator, elements):
			return .Join(transform(separator), elements.map(transform))
		}
	}

	typealias Recur = Payload

	var description: String {
		switch self {
		case .Empty:
			return ""
		case let .Text(s):
			return s
		case let .Horizontal(a):
			return "".join(lazy(a).map { String($0) })
		case let .Vertical(a):
			return "\n".join(lazy(a).map { String($0) })
		case let .Wrap(open, body, close):
			return "\(String(open))\(String(body))\(String(close))"
		case let .Join(separator, elements):
			return String(separator).join(elements.map { String($0) })
		}
	}
}

func == <F: Equatable> (left: Doc<F>, right: Doc<F>) -> Bool {
	switch (left, right) {
	case (.Empty, .Empty):
		return true
	case let (.Text(a), .Text(b)):
		return a == b
	case let (.Horizontal(a), .Horizontal(b)):
		return a == b
	case let (.Vertical(a), .Vertical(b)):
		return a == b
	case let (.Wrap(a1, b1, c1), .Wrap(a2, b2, c2)):
		return a1 == a2 && b1 == b2 && c1 == c2
	case let (.Join(s1, e1), .Join(s2, e2)):
		return s1 == s2 && e1 == e2
	default:
		return false
	}
}


struct Pretty: CustomStringConvertible, Equatable, FixpointType {
	init<T>(_ value: T) {
		self.init((value as? CustomDocConvertible)?.doc ?? .Text(String(value)))
	}

	init(_ doc: Doc<Pretty>) {
		self.init { doc }
	}

	init(_ doc: () -> Doc<Pretty>) {
		self.doc = doc
	}

	let doc: () -> Doc<Pretty>
	var out: Doc<Pretty> {
		return doc()
	}

	var description: String {
		return out.description
	}


	static let Empty = Pretty(Doc<Pretty>.Empty)
	static let Text = Doc<Pretty>.Text >>> Pretty.init
	static let Horizontal = Doc<Pretty>.Horizontal >>> Pretty.init
	static let Vertical = Doc<Pretty>.Vertical >>> Pretty.init
	static let Wrap = Doc<Pretty>.Wrap >>> Pretty.init

	static func Wrap(open: Pretty, _ body: Pretty, _ close: Pretty) -> Pretty {
		return Pretty(.Wrap(open, body, close))
	}

	static func Join(separator: Pretty, _ elements: [Pretty]) -> Pretty {
		return Pretty(.Join(separator, elements))
	}
}

func == (left: Pretty, right: Pretty) -> Bool {
	return true
}


protocol CustomDocConvertible: CustomStringConvertible {
	var doc: Doc<Pretty> { get }
}

extension CustomDocConvertible {
	var description: String {
		return doc.description
	}
}
