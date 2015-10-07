extension Dictionary {
	public init<C: CollectionType where C.Generator.Element == (Key, Value)>(elements: C) {
		self.init(minimumCapacity: Int(elements.count.toIntMax()))
		for (key, value) in elements {
			self[key] = value
		}
	}
}
