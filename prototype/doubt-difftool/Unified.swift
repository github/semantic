func unified(patch: Patch<Term>, source: String) -> String {
	return (patch.state.before.map { "{-\($0)-}" } ?? "")
		+ (patch.state.after.map { "{+\($0)+}" } ?? "")
}

private func range(patch: Patch<Term>) -> Range<Int> {
	return 0..<0
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
				out += String(after.utf16[previous..<range.startIndex])
				out += string
				previous = range.endIndex
			}
			return (out + String(after.utf16[previous..<info.1.range.endIndex]), info.1.range)
		case let .Fixed(f):
			return (f.map { $0.0 }.joinWithSeparator(""), info.1.range)
		case let .Keyed(k):
			return (k.values.map { $0.0 }.joinWithSeparator(""), info.1.range)
		}
	}.0
}


import Doubt
