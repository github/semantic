private func unified(term: Term, source: String) -> String {
	return term.cata { info, syntax -> (String, Range<Int>?) in
		switch syntax {
		case .Leaf:
			return (String(source.utf16[info.range]), info.range)
		case let .Indexed(i):
			return (unified(info.range, children: i, source: source), info.range)
		case let .Fixed(f):
			return (unified(info.range, children: f, source: source), info.range)
		case let .Keyed(k):
			return (unified(info.range, children: k.values.sort(isOrderedBefore), source: source), info.range)
		}
	}.0
}

private func isOrderedBefore(a: (String, Range<Int>?), _ b: (String, Range<Int>?)) -> Bool {
	if let a = a.1, b = b.1 {
		return a.startIndex < b.startIndex
	}
	return false
}

private var isTTY = isatty(STDOUT_FILENO) != 0
private var isDumb = String.fromCString(getenv("TERM")).map { !$0.hasPrefix("dumb") } ?? true
private var shouldFormat = isTTY && !isDumb

private struct Attribute {
	let colour: Colour
	let style: Style

	enum Colour: Int {
		case Black = 30
		case Red
		case Green
		case Yellow
		case Blue
		case Purple
		case Cyan
		case White
	}

	enum Style: Int {
		case Normal = 0
		case Bold = 1
		case Underline = 4
	}

	func wrap(string: String) -> String {
		return shouldFormat
			? "\u{001B}[\(style.rawValue);\(colour.rawValue)m\(string)\u{001B}[0m"
			: string
	}
}

private func unified(patch: Patch<Term>, before: String, after: String) -> String {
	return (patch.state.before.map { Attribute(colour: .Red, style: .Bold).wrap("{-\(unified($0, source: before))-}") } ?? "")
		+ (patch.state.after.map { Attribute(colour: .Green, style: .Bold).wrap("{+\(unified($0, source: after))+}") } ?? "")
}

private func range(patch: Patch<Term>) -> Range<Int>? {
	return patch.state.after?.extract.range
}

private func unified(range: Range<Int>, children: [(String, Range<Int>?)], source: String) -> String {
	var previous = range.startIndex
	var out: String = ""
	for (string, range) in children {
		if let range = range {
			out += String(source.utf16[previous..<range.startIndex])
			previous = range.endIndex
		}
		out += string
	}
	return out + String(source.utf16[previous..<range.endIndex])
}

func unified(diff: Diff, before: String, after: String) -> String {
	return diff.map { (unified($0, before: before, after: after), range($0)) }.cata { info, syntax in
		switch syntax {
		case .Leaf:
			return (String(after.utf16[info.1.range]), info.1.range)
		case let .Indexed(i):
			return (unified(info.1.range, children: i, source: after), info.1.range)
		case let .Fixed(f):
			return (unified(info.1.range, children: f, source: after), info.1.range)
		case let .Keyed(k):
			return (unified(info.1.range, children: k.values.sort(isOrderedBefore), source: after), info.1.range)
		}
	}.0
}


import Doubt
