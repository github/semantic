//  Copyright © 2015 GitHub. All rights reserved.

public enum Cofree<B> {
	case Unroll(B)
}


// MARK: - Comonad

extension Cofree {
	var extract: B {
		switch self {
		case let .Unroll(b):
			return b
		}
	}
}
