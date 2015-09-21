public protocol DictionaryConvertible {
	typealias Key : Hashable
	typealias Value

	init(dictionary: [Key:Value])
	var dictionary: [Key:Value] { get }
}

extension Dictionary: DictionaryConvertible {
	public init(dictionary: [Key:Value]) {
		self = dictionary
	}

	public var dictionary: [Key:Value] {
		return self
	}
}
