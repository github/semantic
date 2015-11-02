func unified(patch: Patch<Term>, source: String) -> String {
	return (patch.state.before.map { "{-\($0)-}" } ?? "")
		+ (patch.state.after.map { "{+\($0)+}" } ?? "")
}

func unified(diff: Diff, before: String, after: String) -> String {
	switch diff {
	case let .Pure(patch):
		return unified(patch, source: after)

	case .Roll:
		return ""
	}
}


import Doubt
