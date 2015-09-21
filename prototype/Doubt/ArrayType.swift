public protocol ArrayType {
	typealias Element

	init(array: [Element])
	var array: [Element] { get }
}

extension Array : ArrayType {
	public init(array: [Element]) {
		self = array
	}

	public var array: [Element] {
		return self
	}
}
