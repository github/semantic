import Cocoa
import Doubt
import Prelude

func readFile(path: String) -> String? {
	guard let data = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) else { return nil }
	return data as String?
}

typealias Term = Cofree<String, Range<Int>>

struct Info: Categorizable, CustomJSONConvertible, Equatable {
	let range: Range<Int>
	let category: Category


	// MARK: Categorizable

	var categories: Set<Category> {
		return [ category ]
	}


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		return [
			"range": [
				range.startIndex.JSON,
				(range.endIndex - range.startIndex).JSON,
			],
			"category": category.rawValue.JSON
		]
	}


	// MARK: Categories

	enum Category: String {
		case Arguments = "arguments"
		case Assignment = "assignment"
		case Comment = "comment"
		case ExpressionStatement = "expression_statement"
		case FormalParameters = "formal_parameters"
		case Function = "function"
		case FunctionCall = "function_call"
		case Identifier = "identifier"
		case IfStatement = "if_statement"
		case MemberAccess = "member_access"
		case NewExpression = "new_expression"
		case NullLiteral = "null"
		case Object = "object"
		case Pair = "pair"
		case Program = "program"
		case RelationalOperator = "rel_op"
		case ReturnStatement = "return_statement"
		case StatementBlock = "statement_block"
		case StringLiteral = "string"
		case SubscriptAccess = "subscript_access"
	}
}

func == (left: Info, right: Info) -> Bool {
	return left.range == right.range && left.category == right.category
}

func termWithInput(string: String) -> Term? {
	let document = ts_document_make()
	defer { ts_document_free(document) }
	return string.withCString {
		ts_document_set_language(document, ts_language_javascript())
		ts_document_set_input_string(document, $0)
		ts_document_parse(document)
		let root = ts_document_root_node(document)

		struct E: ErrorType {}
		return try? Cofree
			.ana { node, category in
				let count = node.namedChildren.count
				guard count > 0 else { return Syntax.Leaf(category.rawValue) }
				switch category {
				case .Pair:
					return try .Fixed(node.namedChildren.map {
						guard let category = Info.Category(rawValue: try node.name(document)) else { throw E() }
						return ($0, category)
					})
				case .Object:
					return try .Keyed(Dictionary(elements: node.namedChildren.map {
						let name = try node.name(document)
						guard let category = Info.Category(rawValue: name) else { throw E() }
						// fixme: this should return the key in the pair.
						return (name, ($0, category))
					}))
				default:
					return try .Indexed(node.namedChildren.map {
						guard let category = Info.Category(rawValue: try node.name(document)) else { throw E() }
						return ($0, category)
					})
				}
			} (root, Info.Category.Program)
			.map { node, category in
				let start = ts_node_pos(node).chars
				return start..<(start + ts_node_size(node).chars)
			}
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
if let aString = arguments[1].flatMap(readFile), bString = arguments[2].flatMap(readFile), c = arguments[3], ui = arguments[4] {
	if let a = termWithInput(aString), b = termWithInput(bString) {
		let diff = Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: const(true), cost: Free.sum(Patch.difference)).run(a, b)
		let range: Range<Int> -> Doubt.JSON = {
			[
				$0.startIndex.JSON,
				($0.endIndex - $0.startIndex).JSON,
			]
		}
		let JSON: Doubt.JSON = [
			"before": .String(aString),
			"after": .String(bString),
			"diff": diff.JSON(pure: { $0.JSON { $0.JSON(annotation: range, leaf: Doubt.JSON.String) } }, leaf: Doubt.JSON.String, annotation: {
				[
					"before": range($0),
					"after": range($1),
				]
			}),
		]
		let data = JSON.serialize()
		try data.writeToFile(c, options: .DataWritingAtomic)

		let components = NSURLComponents()
		components.scheme = "file"
		components.path = ui
		components.query = c
		if let URL = components.URL {
			NSWorkspace.sharedWorkspace().openURL(URL)
		}
	}
}
