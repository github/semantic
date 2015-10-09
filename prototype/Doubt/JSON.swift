public enum JSON: Equatable, FloatLiteralConvertible, IntegerLiteralConvertible, JSONConvertible {
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

	public init?(object: AnyObject) {
		struct E: ErrorType {}
		func die<T>() throws -> T {
			throw E()
		}
		do {
			switch object {
			case let n as Double:
				self = .Number(n)
			case let b as Bool:
				self = .Boolean(b)
			case let s as Swift.String:
				self = .String(s)
			case let a as [AnyObject]:
				self = .Array(try a.map { try Doubt.JSON(object: $0) ?? die() })
			case let d as [Swift.String:AnyObject]:
				self = .Dictionary(Swift.Dictionary(elements: try d.map { ($0, try Doubt.JSON(object: $1) ?? die()) }))
			case is NSNull:
				self = .Null
			default:
				return nil
			}
		} catch { return nil }
	}


	// MARK: FloatLiteralConvertible

	public init(floatLiteral: Double) {
		self = .Number(floatLiteral)
	}


	// MARK: IntegerLiteralConvertible

	public init(integerLiteral: Int) {
		self = .Number(Double(integerLiteral))
	}


	// MARK: JSONConvertible

	public init?(JSON: Doubt.JSON) {
		self = JSON
	}

	public var JSON: Doubt.JSON {
		return self
	}
}


public func == (left: JSON, right: JSON) -> Bool {
	switch (left, right) {
	case let (.Number(a), .Number(b)):
		return a == b
	case let (.Boolean(a), .Boolean(b)):
		return a == b
	case let (.String(a), .String(b)):
		return a == b
	case let (.Array(a), .Array(b)):
		return a == b
	case let (.Dictionary(a), .Dictionary(b)):
		return a == b
	case (.Null, .Null):
		return true
	default:
		return false
	}
}


/// A type which can be converted to JSON.
public protocol CustomJSONConvertible {
	var JSON: Doubt.JSON { get }
}


/// A type which can be converted to and from JSON.
public protocol JSONConvertible: CustomJSONConvertible {
	init?(JSON: Doubt.JSON)
}


import Foundation
