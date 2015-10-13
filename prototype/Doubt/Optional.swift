extension Optional {
	static func equals(param: (Wrapped, Wrapped) -> Bool)(_ left: Wrapped?, _ right: Wrapped?) -> Bool {
		switch (left, right) {
		case let (.Some(a), .Some(b)):
			return param(a, b)
		case (.None, .None):
			return true
		default:
			return false
		}
	}
}
