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

	var children: AnyRandomAccessCollection<TSNode> {
		return AnyRandomAccessCollection(ChildrenCollection(node: self, count: ts_node_child_count, child: ts_node_child))
	}
}
