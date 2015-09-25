public enum Vertex<Element>: CustomDebugStringConvertible, CustomStringConvertible {
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
		func across(vertex: Vertex<Element>) -> Stream<Memo<Vertex<Element>>> {
			return Stream.unfold(Memo(evaluated: vertex)) { vertex in
				vertex.value.analysis(
					ifXY: { _, xs, _ in .Some(vertex, xs) },
					ifEnd: const(.Some(vertex, vertex)))
			}
		}
		self = rows
			.map { a in columns.map { b in combine(a, b) } }
			.fold(.End) {
				$0.zipWith(across($1.value)).fold(.End) {
					.XY($0.0, $1, $0.1)
				}
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


	public var debugDescription: String {
		return rowMajor.map {
			$0.map { String(reflecting: $0) }.joinWithSeparator("\t")
		}.joinWithSeparator("\n")
	}

	public var description: String {
		return rowMajor.map {
			$0.map { String($0) }.joinWithSeparator("\t")
			}.joinWithSeparator("\n")
	}
}
