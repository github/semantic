//  Copyright © 2015 GitHub. All rights reserved.

typealias TSDocument = COpaquePointer

extension TSNode {
	func category(document: TSDocument) throws -> String {
		guard let category = String.fromCString(ts_node_name(self, document)) else { throw "couldn’t make a String from the node name" }
		return category
	}

	var range: Range<Int> {
		let start = ts_node_pos(self).chars
		return start..<(start + ts_node_size(self).chars)
	}

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
