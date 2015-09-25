public enum Vertex<Element> {
	indirect case XY(Element, Memo<Vertex>, Memo<Vertex>)
	case End

	public func analysis<Result>(@noescape ifXY ifXY: (Element, Memo<Vertex>, Memo<Vertex>) -> Result, @noescape ifEnd: () -> Result) -> Result {
		switch self {
		case let .XY(a, x, y):
			return ifXY(a, x, y)
		case .End:
			return ifEnd()
		}
	}

	public var element: Element? {
		return analysis(
			ifXY: { x, _, _ in x },
			ifEnd: const(nil))
	}

	public var right: Memo<Vertex> {
		return analysis(
			ifXY: { _, xs, _ in xs },
			ifEnd: const(Memo(evaluated: .End)))
	}

	public var down: Memo<Vertex> {
		return analysis(
			ifXY: { _, _, ys in ys },
			ifEnd: const(Memo(evaluated: .End)))
	}

	public var diagonal: Memo<Vertex> {
		return right.flatMap { $0.down }
	}


	public var row: Stream<Element> {
		return Stream.unfold(Memo(evaluated: self)) {
			$0.value.analysis(
				ifXY: { here, across, _ in .Some(here, across) },
				ifEnd: const(nil))
		}
	}

	public var column: Stream<Element> {
		return Stream.unfold(Memo(evaluated: self)) {
			$0.value.analysis(
				ifXY: { here, _, down in .Some(here, down) },
				ifEnd: const(nil))
		}
	}

	public var rowMajor: Stream<Stream<Element>> {
		return Stream.unfold(Memo(evaluated: self), { (state: Memo<Vertex>) -> (Stream<Element>, Memo<Vertex>)? in
			state.value.analysis(
				ifXY: { _, _, down in (state.value.row, down) },
				ifEnd: const(nil))
		})
	}

	public var columnMajor: Stream<Stream<Element>> {
		return Stream.unfold(Memo(evaluated: self), { (state: Memo<Vertex>) -> (Stream<Element>, Memo<Vertex>)? in
			state.value.analysis(
				ifXY: { _, across, _ in (state.value.column, across) },
				ifEnd: const(nil))
		})
	}


	public init<S1: SequenceType, S2: SequenceType>(rows: S1, columns: S2, combine: (S1.Generator.Element, S2.Generator.Element) -> Element) {
		let rows = Stream(sequence: rows)
		let columns = Stream(sequence: columns)
		self = columns
			.map { b in rows.map { a in combine(a, b) } }
			.fold(Vertex.End) {
				$0.fold(($1, .End)) {
					($1.flatMap { row, _ in row }.flatMap { $0.right }, .XY($0, $1.map { _, column in column }, $1.flatMap { row, _ in row }))
				}.1
			}
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
