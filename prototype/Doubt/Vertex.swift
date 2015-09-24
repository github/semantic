public enum Vertex<Element> {
	case X(Element, Memo<Vertex>)
	case Y(Element, Memo<Vertex>)
	case XY(Element, Memo<Vertex>, Memo<Vertex>, Memo<Vertex>)
	case End(Element)

	public var row: Stream<Element> {
		switch self {
		case let .X(x, xs):
			return .Cons(x, xs.map { $0.row })
		case let .XY(x, xs, _, _):
			return .Cons(x, xs.map { $0.row })
		default:
			return .Nil
		}
	}

	public var column: Stream<Element> {
		switch self {
		case let .Y(y, ys):
			return .Cons(y, ys.map { $0.column })
		case let .XY(y, _, ys, _):
			return .Cons(y, ys.map { $0.column })
		default:
			return .Nil
		}
	}
}
