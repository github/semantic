public protocol ArrayConvertible {
	typealias Element

	init(array: [Element])
	var array: [Element] { get }
}

extension Array : ArrayConvertible {
	public init(array: [Element]) {
		self = array
	}

	public var array: [Element] {
		return self
	}
}
