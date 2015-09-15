//  Copyright © 2015 GitHub. All rights reserved.

enum Tree<A> {
	case Leaf(A)
	case Branch([Tree])
}
