import Cocoa
import Doubt
import Prelude
import Madness

func benchmark<T>(label: String? = nil, _ f: () -> T) -> T {
	let start = NSDate.timeIntervalSinceReferenceDate()
	let result = f()
	let end = NSDate.timeIntervalSinceReferenceDate()
	print((label.map { "\($0): " } ?? "") + "\(end - start)s")
	return result
}

extension String: ErrorType {}

typealias Term = Cofree<String, Info>
typealias Parser = String throws -> Term

struct Source {
	init(_ argument: String) throws {
		let URL = NSURL(string: argument) ?? NSURL(fileURLWithPath: argument)
		self.URL = URL
		contents = try NSString(contentsOfURL: URL, encoding: NSUTF8StringEncoding) as String
	}

	let URL: NSURL
	var type: String  {
		return URL.pathExtension ?? URL.fragment ?? ""
	}
	let contents: String

	private static let languagesByType: [String:TSLanguage] = [
		"js": ts_language_javascript(),
		"c": ts_language_c(),
		"h": ts_language_c(),
	]
}


extension String.UTF16View {
	subscript (range: Range<Int>) -> String.UTF16View {
		return self[Index(_offset: range.startIndex)..<Index(_offset: range.endIndex)]
	}
}


/// Allow predicates to occur in pattern matching.
func ~= <A> (left: A -> Bool, right: A) -> Bool {
	return left(right)
}


func termWithInput(language: TSLanguage)(_ string: String) throws -> Term {
	let keyedProductions: Set<String> = [ "object" ]
	let fixedProductions: Set<String> = [ "pair", "rel_op", "math_op", "bool_op", "bitwise_op", "type_op", "math_assignment", "assignment", "subscript_access", "member_access", "new_expression", "function_call", "function", "ternary" ]
	let document = ts_document_make()
	defer { ts_document_free(document) }
	return try string.withCString {
		ts_document_set_language(document, language)
		ts_document_set_input_string(document, $0)
		ts_document_parse(document)
		let root = ts_document_root_node(document)

		return try Cofree
			.ana { node, category in
				let count = node.namedChildren.count
				guard count > 0 else { return try Syntax.Leaf(node.substring(string)) }
				switch category {
				case fixedProductions.contains:
					return try .Fixed(node.namedChildren.map {
						($0, try $0.category(document))
					})
				case keyedProductions.contains:
					return try .Keyed(Dictionary(elements: node.namedChildren.map {
						switch try $0.category(document) {
						case "pair":
							return try ($0.namedChildren[0].substring(string), ($0, "pair"))
						default:
							// We might have a comment inside an object literal. It should still be assigned a key, however.
							return try (try node.substring(string), ($0, $0.category(document)))
						}
					}))
				default:
					return try .Indexed(node.namedChildren.map {
						($0, try $0.category(document))
					})
				}
			} (root, "program")
			.map { node, category in
				Info(range: node.range, categories: [ category ])
			}
	}
}

func toTerm(term: CofreeJSON) -> Term {
	let annotation = Info(range: term.extract, categories: [])
	switch term.unwrap {
	case let .Leaf(a):
		return Term(Info(range: term.extract, categories: a.categories), Syntax<Term, String>.Leaf(String(a)))
	case let .Indexed(i):
		return Term(annotation, .Indexed(i.map(toTerm)))
	case let .Fixed(f):
		return Term(annotation, .Fixed(f.map(toTerm)))
	case let .Keyed(k):
		return Term(annotation, .Keyed(Dictionary(elements: k.map { ($0, toTerm($1)) })))
	}
}

func parserForType(type: String) -> (String throws -> Term)? {
	switch type {
	case "json":
		return { (input: String) throws -> Term in
			switch parse(json, input: input.characters) {
			case let .Right(term):
				return toTerm(term)
			case let .Left(error):
				throw error.description
			}
		}
	default:
		return Source.languagesByType[type].map(termWithInput)
	}
}

let arguments = BoundsCheckedArray(array: Process.arguments)
guard let aSource = try arguments[1].map(Source.init) else { throw "need source A" }
guard let bSource = try arguments[2].map(Source.init) else { throw "need source B" }
let jsonURL = NSURL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).URLByAppendingPathComponent("diff.json")
guard let uiPath = NSBundle.mainBundle().infoDictionary?["PathToUISource"] as? String else { throw "need ui path" }
guard aSource.type == bSource.type else { throw "can’t compare files of different types" }
guard let parser = parserForType(aSource.type) else { throw "don’t know how to parse files of type \(aSource.type)" }

let a = try parser(aSource.contents)
let b = try parser(bSource.contents)
let diff = Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: Interpreter<Term>.comparable { $0.extract.categories }, cost: Free.sum(Patch.sum)).run(a, b)
let JSON: Doubt.JSON = [
	"before": .String(aSource.contents),
	"after": .String(bSource.contents),
	"diff": diff.JSON(pure: { $0.JSON { $0.JSON(annotation: { $0.range.JSON }, leaf: Doubt.JSON.String) } }, leaf: Doubt.JSON.String, annotation: {
		[
			"before": $0.range.JSON,
			"after": $1.range.JSON,
		]
	}),
]
let data = JSON.serialize()
try data.writeToURL(jsonURL, options: .DataWritingAtomic)

let components = NSURLComponents()
components.scheme = "file"
components.path = uiPath
components.query = jsonURL.absoluteString
if let URL = components.URL {
	NSWorkspace.sharedWorkspace().openURL(URL)
}
