private func unified(patch: Patch<Term>, source: String) -> String {
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
	return diff.map { (unified($0, source: after), range($0)) }.cata { info, syntax in
		switch syntax {
		case .Leaf:
			return (String(after.utf16[info.1.range]), info.1.range)
		case let .Indexed(i):
			var previous = info.1.range.startIndex
			var out: String = ""
			for (string, range) in i {
				if let range = range {
					out += String(after.utf16[previous..<range.startIndex])
					previous = range.endIndex
				}
				out += string
			}
			return (out + String(after.utf16[previous..<info.1.range.endIndex]), info.1.range)
		case let .Fixed(f):
			return (unified(info.1.range, children: f, source: after), info.1.range)
		case let .Keyed(k):
			return (k.values.map { $0.0 }.joinWithSeparator(""), info.1.range)
		}
	}.0
}


import Doubt
