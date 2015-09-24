public enum Vertex<Element> {
	case XY(Element, Memo<Vertex>, Memo<Vertex>)
	case End


	public var right: Memo<Vertex> {
		switch self {
		case let .XY(_, xs, _):
			return xs
		case .End:
			return Memo(evaluated: .End)
		}
	}

	public var down: Memo<Vertex> {
		switch self {
		case let .XY(_, _, ys):
			return ys
		case .End:
			return Memo(evaluated: .End)
		}
	}

	public var diagonal: Memo<Vertex> {
		return right.flatMap { $0.down }
	}


	public func map<Other>(transform: Element -> Other) -> Vertex<Other> {
		switch self {
		case let .XY(xy, xs, ys):
			return .XY(transform(xy), xs.map { $0.map(transform) }, ys.map { $0.map(transform) })
		case .End:
			return .End
		}
	}
}
