/// A type which can be converted to and from JSON.
public protocol JSONConvertible {
	init?(JSON: Doubt.JSON)
	var JSON: Doubt.JSON { get }
}
