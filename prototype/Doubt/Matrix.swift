struct Matrix<A> {
	init(width: Int, height: Int, compute: (Int, Int) -> A) {
		var values: [Memo<A>] = []
		values.reserveCapacity(width * height)

		for i in 0..<width {
			for j in 0..<height {
				values[i + j * height] = Memo<A> { compute(i, j) }
			}
		}

		self.init(width: width, height: height, values: values)
	}

	let width: Int
	let height: Int

	let values: [Memo<A>]

	subscript (i: Int, j: Int) -> Memo<A>? {
		guard i < width && j < height else { return nil }
		return values[i + j * height]
	}


	// MARK: Implementation details

	private init(width: Int, height: Int, values: [Memo<A>]) {
		self.width = width
		self.height = height
		self.values = values
	}
}
