//  Copyright © 2015 GitHub. All rights reserved.

extension TSNode {
	private struct ChildrenCollection: CollectionType {
		let node: TSNode
		let count: TSNode -> Int
		let child: (TSNode, Int) -> TSNode

		subscript (index: Int) -> TSNode {
			return child(node, index)
		}

		let startIndex = 0
		var endIndex: Int {
			return count(node)
		}
	}
}
