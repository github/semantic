enum Argument {
	case File(Source)
}

let argumentsParser: Madness.Parser<[String], Argument>.Function = none()


import Madness
