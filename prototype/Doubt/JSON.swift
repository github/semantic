public enum JSON {
	public typealias ArrayType = [Doubt.JSON]
	public typealias DictionaryType = [Swift.String:Doubt.JSON]

	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Array(ArrayType)
	case Dictionary(DictionaryType)
	case Null


	public var number: Double? {
		if case let .Number(d) = self { return d }
		return nil
	}

	public var boolean: Bool? {
		if case let .Boolean(b) = self { return b }
		return nil
	}

	public var string: Swift.String? {
		if case let .String(s) = self { return s }
		return nil
	}

	public var array: ArrayType? {
		if case let .Array(a) = self { return a }
		return nil
	}

	public var dictionary: DictionaryType? {
		if case let .Dictionary(d) = self { return d }
		return nil
	}

	public var isNull: Bool {
		if case .Null = self { return true }
		return false
	}

	public static let JSON: Prism<AnyObject, Doubt.JSON> = Prism(forward: toJSON, backward: toAnyObject)
}

protocol JSONConvertible {
	init(JSON: Doubt.JSON)
	var JSON: Doubt.JSON { get }
}

extension JSON: JSONConvertible {
	init(JSON: Doubt.JSON) {
		self = JSON
	}

	var JSON: Doubt.JSON {
		return self
	}
}

extension Prism where To : JSONConvertible {
	var number: Prism<From, Double> {
		return self >>> Prism<To, Double>(forward: { $0.JSON.number }, backward: { To(JSON: .Number($0)) })
	}

	var boolean: Prism<From, Bool> {
		return self >>> Prism<To, Bool>(forward: { $0.JSON.boolean }, backward: { To(JSON: .Boolean($0)) })
	}

	var string: Prism<From, Swift.String> {
		return self >>> Prism<To, Swift.String>(forward: { $0.JSON.string }, backward: { To(JSON: .String($0)) })
	}

	var array: Prism<From, [Doubt.JSON]> {
		return self >>> Prism<To, [Doubt.JSON]>(forward: { $0.JSON.array }, backward: { To(JSON: .Array($0)) })
	}

	var dictionary: Prism<From, [String:Doubt.JSON]> {
		return self >>> Prism<To, [String:Doubt.JSON]>(forward: { $0.JSON.dictionary }, backward: { To(JSON: .Dictionary($0)) })
	}
}

protocol DictionaryType {
	typealias Key : Hashable
	typealias Value

	init(dictionary: [Key:Value])
	var dictionary: [Key:Value] { get }
}

extension Dictionary: DictionaryType {
	init(dictionary: [Key:Value]) {
		self = dictionary
	}

	var dictionary: [Key:Value] {
		return self
	}
}

extension Prism where To : DictionaryType {
	subscript (key: To.Key) -> Prism<From, To.Value> {
		return self >>> Prism<To, To.Value>(forward: { $0.dictionary[key] }, backward: { To(dictionary: [key: $0]) })
	}
}

protocol ArrayType {
	typealias Element

	init(array: [Element])
	var array: [Element] { get }
}

extension Array : ArrayType {
	init(array: [Element]) {
		self = array
	}

	var array: [Element] {
		return self
	}
}

extension Prism where To : ArrayType {
	subscript (index: Int) -> Prism<From, To.Element> {
		return self >>> Prism<To, To.Element>(
			forward: {
				let array = $0.array
				return array.count > index ? array[index] : nil
			},
			backward: { To(array: [ $0 ]) })
	}
}

private func toJSON(object: AnyObject) -> JSON? {
	struct E: ErrorType {}
	func die<T>() throws -> T {
		throw E()
	}
	do {
		switch object {
		case let n as Double:
			return JSON.Number(n)
		case let b as Bool:
			return JSON.Boolean(b)
		case let s as String:
			return JSON.String(s)
		case let a as [AnyObject]:
			return JSON.Array(try a.map { try toJSON($0) ?? die() })
		case let d as [String:AnyObject]:
			return JSON.Dictionary(Dictionary(elements: try d.map { ($0, try toJSON($1) ?? die()) }))
		case is NSNull:
			return JSON.Null
		default:
			return nil
		}
	} catch { return nil }
}

private func toAnyObject(json: JSON) -> AnyObject {
	switch json {
	case let .Number(n):
		return n
	case let .Boolean(b):
		return b
	case let .String(s):
		return s
	case let .Array(a):
		return a.map(toAnyObject)
	case let .Dictionary(d):
		return Dictionary(elements: d.map { ($0, toAnyObject($1)) })
	case .Null:
		return NSNull()
	}
}


import Foundation
