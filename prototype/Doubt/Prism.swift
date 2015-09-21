struct Prism<From, To> {
	let forward: From -> To?
	let backward: To -> From
}

extension Dictionary {
	static func prism(key: Key) -> Prism<[Key:Value], Value> {
		return Prism(forward: { $0[key] }, backward: { [key: $0] })
	}
}

func >>> <From, Part, To> (left: Prism<From, Part>, right: Prism<Part, To>) -> Prism<From, To> {
	return Prism(forward: { left.forward($0).flatMap(right.forward) }, backward: right.backward >>> left.backward)
}
