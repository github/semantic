//  Copyright © 2015 GitHub. All rights reserved.

public enum Cofree<A, B> {
	case Unroll(B, Syntax<Cofree, A>)
}


// MARK: - Comonad

extension Cofree {
	var extract: B {
		switch self {
		case let .Unroll(b, _):
			return b
		}
	}
}
