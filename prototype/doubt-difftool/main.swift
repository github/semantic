import Cocoa
import Doubt
import Prelude
import Madness

func benchmark<T>(label: String? = nil, _ f: () throws -> T) rethrows -> T {
	let start = NSDate.timeIntervalSinceReferenceDate()
	let result = try f()
	let end = NSDate.timeIntervalSinceReferenceDate()
	print((label.map { "\($0): " } ?? "") + "\(end - start)s")
	return result
}

extension String: ErrorType {}

typealias Term = Cofree<String, Info>
typealias Diff = Free<Term.Leaf, (Term.Annotation, Term.Annotation), Patch<Term>>
typealias Parser = String throws -> Term

struct Source {
	init(_ argument: String) throws {
		let supportedSchemes = [ "http", "https", "file" ]
		if let URL = NSURL(string: argument) where supportedSchemes.contains(URL.scheme) {
			self.URL = URL
		} else {
			self.URL = NSURL(fileURLWithPath: argument)
		}
		contents = try NSString(contentsOfURL: URL, encoding: NSUTF8StringEncoding) as String
	}

	let URL: NSURL
	var type: String  {
		if let pathExtension = URL.pathExtension where pathExtension != "" { return pathExtension }
		return URL.fragment ?? ""
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

func lines(input: String) -> Term {
	var lines: [Term] = []
	var previous = 0
	input.enumerateSubstringsInRange(input.characters.indices, options: .ByLines) { (line, _, enclosingRange, _) in
		let range: Range<Int> = previous..<(previous + enclosingRange.count)
		previous = range.endIndex
		if let line = line {
			lines.append(Term(Info(range: range, categories: []), Syntax.Leaf(line)))
		}
	}
	return Term(Info(range: 0..<input.utf16.count, categories: []), .Indexed(lines))
}

func parserForType(type: String) -> String throws -> Term {
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
		if let parser = Source.languagesByType[type].map(termWithInput) {
			return parser
		}
		return lines
	}
}

extension ForwardIndexType {
	/// The range encompassing a single index.
	var range: Range<Self> {
		return self..<self.successor()
	}
}

func refineLeafReplacement(aString: String, _ bString: String)(_ patch: Patch<Term>) -> Diff {
	switch patch {
	case let .Replace(.Unroll(aExtract, .Leaf), .Unroll(bExtract, .Leaf)):
		let a = aString.utf16[aExtract.range].enumerate().map { Term(Info(range: (aExtract.range.startIndex + 0).range, categories: aExtract.categories), .Leaf(String($1))) }
		let b = bString.utf16[bExtract.range].enumerate().map { Term(Info(range: (bExtract.range.startIndex + 0).range, categories: bExtract.categories), .Leaf(String($1))) }
		return .Roll((aExtract, bExtract), .Indexed(SES(a, b, cost: const(1), recur: const(nil))))
	default:
		return .Pure(patch)
	}
}

let parsed = benchmark("parsing arguments & loading sources") { parse(argumentsParser, input: Process.arguments) }
let arguments: Argument = try parsed.either(ifLeft: { throw "\($0)" }, ifRight: { $0 })
let (aSource, bSource) = arguments.sources
let jsonURL = NSURL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).URLByAppendingPathComponent("diff.json")
guard let uiPath = NSBundle.mainBundle().infoDictionary?["PathToUISource"] as? String else { throw "need ui path" }
guard aSource.type == bSource.type else { throw "can’t compare files of different types" }
let parser = parserForType(aSource.type)

let a = try benchmark("parsing source a") { try parser(aSource.contents) }
let b = try benchmark("parsing source b") { try parser(bSource.contents) }
let diff = benchmark("diffing") { Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: Interpreter<Term>.comparable { $0.extract.categories }, cost: Free.sum(Patch.sum)).run(a, b) }
switch arguments.output {
case .Split:
	let JSON: Doubt.JSON = [
		"before": .String(aSource.contents),
		"after": .String(bSource.contents),
		"diff": diff.JSON(pure: { $0.JSON { $0.JSON(annotation: { $0.JSON }, leaf: Doubt.JSON.String) } }, leaf: Doubt.JSON.String, annotation: {
			[
				"before": $0.JSON,
				"after": $1.JSON,
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
case .Unified:
	print(benchmark("formatting unified diff") { unified(diff, before: aSource.contents, after: bSource.contents) })
}
