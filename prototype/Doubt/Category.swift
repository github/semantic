public enum Category: Equatable {
	case Tag(String)
}

public func == (left: Category, right: Category) -> Bool {
	switch (left, right) {
	case let (.Tag(a), .Tag(b)):
		return a == b
	}
}
