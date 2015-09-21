enum SwiftAST: Equatable {
	case Atom(String)
	case Symbol(String, [String])
	case KeyValue(String, String)
	case Branch(String, [SwiftAST])
}
