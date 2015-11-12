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
				// TODO: Calculate line and column from TSNodes
				Info(range: node.range, lines: 0..<1, columns: 0..<1, categories: [ category ])
			}
	}
}

func toTerm(term: CofreeJSON) -> Term {
	let lines = term.extract.0
	let columns = term.extract.1
	let range = term.extract.2
	let annotation = Info(range: range, lines: lines, columns: columns, categories: [])
	switch term.unwrap {
	case let .Leaf(a):
		return Term(Info(range: range, lines: lines, columns: columns, categories: a.categories), Syntax<Term, String>.Leaf(String(a)))
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
	var lineNumber = 0
	input.enumerateSubstringsInRange(input.characters.indices, options: .ByLines) { (line, _, enclosingRange, _) in
		let range: Range<Int> = previous..<(previous + enclosingRange.count)
		previous = range.endIndex
		if let line = line {
			lineNumber += 1
			lines.append(Term(Info(range: range, lines: 0..<lineNumber, columns: 0..<1, categories: []), Syntax.Leaf(line)))
		}
	}
	return Term(Info(range: 0..<input.utf16.count, lines: 0..<lineNumber, columns: 0..<1, categories: []), .Indexed(lines))
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

let cost = Diff.sum(Patch.sum(Term.size))

func refineLeafReplacement(aString: String, _ bString: String)(_ patch: Patch<Term>) -> Diff {
	guard case let .Replace(.Unroll(aExtract, .Leaf), .Unroll(bExtract, .Leaf)) = patch else { return .Pure(patch) }
	let a = aString.utf16[aExtract.range].enumerate().map { Term(Info(range: (aExtract.range.startIndex + $0).range, lines: aExtract.lines, columns: aExtract.columns, categories: aExtract.categories), .Leaf(String($1))) }
	let b = bString.utf16[bExtract.range].enumerate().map { Term(Info(range: (bExtract.range.startIndex + $0).range, lines: bExtract.lines, columns: bExtract.columns, categories: bExtract.categories), .Leaf(String($1))) }
	return .Roll((aExtract, bExtract), .Indexed(SES(a, b, cost: cost, recur: { Term.equals(annotation: const(true), leaf: ==)($0, $1) ? Term.zip($0, $1).map(Diff.init) : Diff.Replace($0, $1) })))
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
let initialDiff = benchmark("diffing") { Interpreter<Term>(equal: Term.equals(annotation: const(true), leaf: ==), comparable: Interpreter<Term>.comparable { $0.extract.categories }, cost: cost).run(a, b) }
let diff = benchmark("diffing within leaves") { initialDiff.flatMap(refineLeafReplacement(aSource.contents, bSource.contents)) }
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
