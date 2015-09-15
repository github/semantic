//  Copyright © 2015 GitHub. All rights reserved.

public enum Tree<A>: CustomStringConvertible {
	case Leaf(A)
	case Branch([Tree])

	public var description: String {
		switch self {
		case let .Leaf(value):
			return String(value)
		case let .Branch(children):
			return "(" + children.lazy.map { String($0) }.joinWithSeparator(" ") + ")"
		}
	}
}
