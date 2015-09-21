public struct Prism<From, To> {
	public init(forward: From -> To?, backward: To -> From) {
		self.forward = forward
		self.backward = backward
	}

	public let forward: From -> To?
	public let backward: To -> From
}

extension Dictionary {
	static func prism(key: Key) -> Prism<[Key:Value], Value> {
		return Prism(forward: { $0[key] }, backward: { [key: $0] })
	}
}

public func >>> <From, Part, To> (left: Prism<From, Part>, right: Prism<Part, To>) -> Prism<From, To> {
	return Prism(forward: { left.forward($0).flatMap(right.forward) }, backward: right.backward >>> left.backward)
}


public protocol DictionaryType {
	typealias Key : Hashable
	typealias Value

	init(dictionary: [Key:Value])
	var dictionary: [Key:Value] { get }
}

extension Dictionary: DictionaryType {
	public init(dictionary: [Key:Value]) {
		self = dictionary
	}

	public var dictionary: [Key:Value] {
		return self
	}
}

extension Prism where To : DictionaryType {
	public subscript (key: To.Key) -> Prism<From, To.Value> {
		return self >>> Prism<To, To.Value>(forward: { $0.dictionary[key] }, backward: { To(dictionary: [key: $0]) })
	}
}

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

extension Prism where To : ArrayType {
	public subscript (index: Int) -> Prism<From, To.Element> {
		return self >>> Prism<To, To.Element>(
			forward: {
				let array = $0.array
				return array.count > index ? array[index] : nil
			},
			backward: { To(array: [ $0 ]) })
	}
}
