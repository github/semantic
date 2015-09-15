public enum Doc: CustomStringConvertible, Equatable {
	case Empty
	case Text(String)
	case Horizontal([Doc])
	case Vertical([Doc])
	indirect case Wrap(Doc, Doc, Doc)
	indirect case Join(Doc, [Doc])

	public init<T>(_ value: T) {
		self.init((value as? CustomDocConvertible)?.doc ?? .Text(String(value)))
	}

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


public protocol CustomDocConvertible: CustomStringConvertible {
	var doc: Doc { get }
}

extension CustomDocConvertible {
	public var description: String {
		return doc.description
	}
}
