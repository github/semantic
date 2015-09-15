//  Copyright © 2015 GitHub. All rights reserved.

public enum Tree<A> {
	case Leaf(A)
	case Branch([Tree])
}
