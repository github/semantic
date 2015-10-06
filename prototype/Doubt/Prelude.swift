func fix<T, U>(f: (T -> U) -> T -> U) -> T -> U {
	return { f(fix(f))($0) }
}


func id<A>(a: A) -> A {
	return a
}

func const<A, B>(a: A)(_ b: B) -> A {
	return a
}


func >>> <T, U, V> (f: T -> U, g: U -> V) -> T -> V {
	return { g(f($0)) }
}


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
