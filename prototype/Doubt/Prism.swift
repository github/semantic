public struct Prism<From, To> {
	public init(forward: From -> To?, backward: To -> From) {
		self.forward = forward
		self.backward = backward
	}

	public let forward: From -> To?
	public let backward: To -> From
}

public func >>> <From, Part, To> (left: Prism<From, Part>, right: Prism<Part, To>) -> Prism<From, To> {
	return Prism(forward: { left.forward($0).flatMap(right.forward) }, backward: right.backward >>> left.backward)
}


extension Prism where To : DictionaryConvertible {
	public subscript (key: To.Key) -> Prism<From, To.Value> {
		return self >>> Prism<To, To.Value>(forward: { $0.dictionary[key] }, backward: { To(dictionary: [key: $0]) })
	}
}

extension Prism where To : ArrayConvertible {
	public subscript (index: Int) -> Prism<From, To.Element> {
		return self >>> Prism<To, To.Element>(
			forward: {
				let array = $0.array
				return array.count > index ? array[index] : nil
			},
			backward: { To(array: [ $0 ]) })
	}

	public func map<A>(transform: Prism<To.Element, A>) -> Prism<From, [A]> {
		return Prism<From, [A]>(
			forward: { self.forward($0)?.array.flatMap(transform.forward) },
			backward: { self.backward(To(array: $0.map(transform.backward))) })
	}

	public func map<A>(transform: Prism<To.Element, To.Element> -> Prism<To.Element, A>) -> Prism<From, [A]> {
		return map(transform(Prism<To.Element, To.Element>(forward: { $0 }, backward: { $0 })))
	}
}

public func &&& <From, A, B> (left: Prism<From, A>, right: Prism<From, B>) -> Prism<From, (A, B)> {
	return Prism(
		forward: {
			if let a = left.forward($0), b = right.forward($0) {
				return (a, b)
			}
			return nil
		},
		backward: {
			(left.backward($0), right.backward($1)).1
		})
}
