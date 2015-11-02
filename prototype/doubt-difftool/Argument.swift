enum Argument {
	indirect case File(Source, Argument)
	case End

	var rest: Argument? {
		switch self {
		case let .File(_, rest):
			return rest
		case .End:
			return nil
		}
	}

	var files: [Source] {
		switch self {
		case let .File(a, rest):
			return [a] + rest.files
		case .End:
			return []
		}
	}
}

let argumentsParser: Madness.Parser<[String], Argument>.Function = none()


import Madness
