struct Matrix<A> {
	init(width: Int, height: Int, compute: (Int, Int) -> A) {
		self.width = width
		self.height = height
		var values: [Memo<A>] = []
		values.reserveCapacity(width * height)

		for i in 0..<width {
			for j in 0..<height {
				values[i + j * height] = Memo<A> { compute(i, j) }
			}
		}

		self.values = values
	}

	let width: Int
	let height: Int

	let values: [Memo<A>]

	subscript (i: Int, j: Int) -> Memo<A>? {
		guard i < width && j < height else { return nil }
		return values[i + j * height]
	}
}
