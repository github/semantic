public enum Vertex<Element> {
	indirect case XY(Element, Memo<Vertex>, Memo<Vertex>)
	case End

	public var element: Element? {
		switch self {
		case let .XY(a, _, _):
			return a
		case .End:
			return nil
		}
	}

	public var right: Memo<Vertex> {
		switch self {
		case let .XY(_, xs, _):
			return xs
		case .End:
			return Memo(.End)
		}
	}

	public var down: Memo<Vertex> {
		switch self {
		case let .XY(_, _, ys):
			return ys
		case .End:
			return Memo(.End)
		}
	}

	public var diagonal: Memo<Vertex> {
		return right.flatMap { $0.down }
	}


	public init<A, B>(rows: Stream<A>, columns: Stream<B>, combine: (A, B) -> Element) {
		self = columns
			.map { b in rows.map { a in (a, b) } }
			.fold(Vertex<(A, B)>.End) {
				$0.fold(($1, .End)) {
					($1.flatMap { row, _ in row }.flatMap { $0.right }, .XY($0, $1.map { _, column in column }, $1.flatMap { row, _ in row }))
				}.1
			}
			.map(combine)
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
