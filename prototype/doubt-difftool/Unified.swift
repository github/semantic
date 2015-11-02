func unified(diff: Interpreter<Term>.Diff, before: String, after: String) -> String {
	switch diff {
	case let .Pure(patch):
		return (patch.state.before.map { "{-\($0)-}" } ?? "")
			+ (patch.state.after.map { "{+\($0)+}" } ?? "")

	case .Roll:
		return ""
	}
}


import Doubt
