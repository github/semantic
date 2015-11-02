/// A list of arguments for the difftool.
enum Argument {
	indirect case File(Source, Argument)
	indirect case OutputFlag(Output, Argument)
	case End

	private var rest: Argument? {
		switch self {
		case let .File(_, rest):
			return rest
		case let .OutputFlag(_, rest):
			return rest
		case .End:
			return nil
		}
	}

	var files: [Source] {
		switch self {
		case let .File(a, rest):
			return [a] + rest.files
		default:
			return rest?.files ?? []
		}
	}

	enum Output {
		case Unified
		case Split
	}
}

private let flag: Madness.Parser<[String], Argument.Output>.Function =
		const(Argument.Output.Unified) <^> satisfy { $0 == "--unified" }
	<|> const(Argument.Output.Split) <^> satisfy { $0 == "--split" }

private let source: Madness.Parser<[String], Source>.Function =
	{ try! Source($0) } <^> satisfy { !$0.hasPrefix("--") }

let argumentsParser: Madness.Parser<[String], Argument>.Function = fix { rest in
	(curry(Argument.OutputFlag) <^> flag <|> curry(Argument.File) <^> source)
		<*> rest
		<|> pure(Argument.End)
}

import Madness
import Prelude
