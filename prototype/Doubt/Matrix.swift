struct Matrix<A> {
	let width: Int
	let height: Int

	let values: [Memo<A>]

	subscript (i: Int, j: Int) -> Memo<A>? {
		guard i < width && j < height else { return nil }
		return values[i + j * height]
	}
}
