func fix<T, U>(f: (T -> U) -> T -> U) -> T -> U {
	return { f(fix(f))($0) }
}


func const<A, B>(a: A)(_ b: B) -> A {
	return a
}


func >>> <T, U, V> (f: T -> U, g: U -> V) -> T -> V {
	return { g(f($0)) }
}

