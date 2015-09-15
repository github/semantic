public enum Doc<Payload>: CustomStringConvertible {
	case Empty
	case Text(String)
	case Horizontal([Payload])
	case Vertical([Payload])
	case Wrap(Payload, Payload, Payload)
	case Join(Payload, [Payload])

	public func map<Other>(@noescape transform: Payload -> Other) -> Doc<Other> {
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

	public typealias Recur = Payload

	public var description: String {
		switch self {
		case .Empty:
			return ""
		case let .Text(s):
			return s
		case let .Horizontal(a):
			return a.lazy.map { String($0) }.joinWithSeparator("")
		case let .Vertical(a):
			return a.lazy.map { String($0) }.joinWithSeparator("\n")
		case let .Wrap(open, body, close):
			return "\(String(open))\(String(body))\(String(close))"
		case let .Join(separator, elements):
			return elements.map { String($0) }.joinWithSeparator(String(separator))
		}
	}
}


public struct Pretty: CustomStringConvertible, Equatable {
	public init<T>(_ value: T) {
		self.init((value as? CustomDocConvertible)?.doc ?? .Text(String(value)))
	}

	public init(_ doc: Doc<Pretty>) {
		self.init { doc }
	}

	public init(_ doc: () -> Doc<Pretty>) {
		self.doc = doc
	}

	let doc: () -> Doc<Pretty>
	public var out: Doc<Pretty> {
		return doc()
	}

	public var description: String {
		return out.description
	}


	public static let Empty = Pretty(Doc<Pretty>.Empty)
	public static let Text = Doc<Pretty>.Text >>> Pretty.init
	public static let Horizontal = Doc<Pretty>.Horizontal >>> Pretty.init
	public static let Vertical = Doc<Pretty>.Vertical >>> Pretty.init
	public static let Wrap = Doc<Pretty>.Wrap >>> Pretty.init

	public static func Wrap(open: Pretty, _ body: Pretty, _ close: Pretty) -> Pretty {
		return Pretty(.Wrap(open, body, close))
	}

	public static func Join(separator: Pretty, _ elements: [Pretty]) -> Pretty {
		return Pretty(.Join(separator, elements))
	}
}


public protocol CustomDocConvertible: CustomStringConvertible {
	var doc: Doc<Pretty> { get }
}

extension CustomDocConvertible {
	public var description: String {
		return doc.description
	}
}
