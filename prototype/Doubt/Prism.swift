public struct Prism<From, To> {
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
