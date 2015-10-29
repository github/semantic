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

	func substring(string: String) throws -> String {
		guard let result = String(string.utf16[range]) else { throw "could not make a string from utf16 range '\(range)'" }
		return result
	}

	var children: ChildrenCollection {
		return ChildrenCollection(node: self, count: ts_node_child_count(self), child: ts_node_child)
	}

	var namedChildren: ChildrenCollection {
		return ChildrenCollection(node: self, count: ts_node_named_child_count(self), child: ts_node_named_child)
	}


	struct ChildrenCollection: CollectionType {
		let node: TSNode
		let count: Int
		let child: (TSNode, Int) -> TSNode

		subscript (index: Int) -> TSNode {
			return child(node, index)
		}

		let startIndex = 0
		var endIndex: Int {
			return count
		}
	}
}
