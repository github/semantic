public enum Location: Equatable {
	/// A literal string, vs. range information.
	case Literal(String)
}

public func == (left: Location, right: Location) -> Bool {
	switch (left, right) {
	case let (.Literal(s1), .Literal(s2)):
		return s1 == s2
	}
}
