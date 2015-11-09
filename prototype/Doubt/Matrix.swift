/// A two-dimensional matrix of memoized values.
///
/// These values are populated by a function from the coordinates of a given cell to the matrix’s element type.
///
/// Values are retrieved by subscripting with row/column indices. Out-of-bound indices produce `nil` values, rather than asserting.
public struct Matrix<A, I: ForwardIndexType> {
	public init(across: Range<I>, down: Range<I>, compute: (I, I) -> A) {
		self.init(across: across, down: down, values: constructRowMajor(across, down: down, forEach: { i, j in Memo { compute(i, j) } }))
	}

	public let across: Range<I>
	public let down: Range<I>

	private let values: [Memo<A>]

	public subscript (i: I, j: I) -> Memo<A>? {
		guard across.contains(i) && down.contains(j) else { return nil }
		let i = across.startIndex.distanceTo(i)
		let j = down.startIndex.distanceTo(j)
		return values[Int((i + j * across.count).toIntMax())]
	}


	// MARK: Functor

	public func map<Other>(transform: A -> Other) -> Matrix<Other, I> {
		return Matrix<Other, I>(across: across, down: down, values: values.map { $0.map(transform) })
	}


	// MARK: Implementation details

	private init(across: Range<I>, down: Range<I>, values: [Memo<A>]) {
		self.across = across
		self.down = down
		self.values = values
	}
}

/// Constructs a row-major ordering of values produced with `forEach`.
private func constructRowMajor<A, I: ForwardIndexType>(across: Range<I>, down: Range<I>, @noescape forEach: (I, I) -> A) -> [A] {
	var values: [A] = []
	values.reserveCapacity(Int(across.count.toIntMax()) * Int(down.count.toIntMax()))
	for j in down {
		for i in across {
			values.append(forEach(i, j))
		}
	}
	return values
}


import Memo
