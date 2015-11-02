enum Argument {
	indirect case File(Source, Argument)
	case End
}

let argumentsParser: Madness.Parser<[String], Argument>.Function = none()


import Madness
