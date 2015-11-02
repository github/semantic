private func unified(term: Term, source: String) -> String {
	return term.cata { info, syntax -> (String, Range<Int>?) in
		switch syntax {
		case .Leaf:
			return (String(source.utf16[info.range]), info.range)
		case let .Indexed(i):
			return (unified(info.range, children: i, source: source), info.range)
		default:
			return ("", info.range)
		}
	}.0
}

private func unified(patch: Patch<Term>, before: String, after: String) -> String {
	return (patch.state.before.map { "{-\($0)-}" } ?? "")
		+ (patch.state.after.map { "{+\($0)+}" } ?? "")
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
			return (unified(info.1.range, children: k.values.sort { a, b in
				if let a = a.1, b = b.1 {
					return a.startIndex < b.startIndex
				}
				return false
			}, source: after), info.1.range)
		}
	}.0
}


import Doubt
