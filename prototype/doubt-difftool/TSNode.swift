//  Copyright © 2015 GitHub. All rights reserved.

typealias TSDocument = COpaquePointer

extension TSNode {
	var children: AnyRandomAccessCollection<TSNode> {
		return AnyRandomAccessCollection(ChildrenCollection(node: self, count: ts_node_child_count, child: ts_node_child))
	}

	var namedChildren: AnyRandomAccessCollection<TSNode> {
		return AnyRandomAccessCollection(ChildrenCollection(node: self, count: ts_node_named_child_count, child: ts_node_named_child))
	}


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
