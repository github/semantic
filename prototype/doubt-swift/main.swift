import Doubt
import SourceKittenFramework

let arguments = BoundsCheckedArray(array: Process.arguments)

extension Term {
	init?(path: String) {
		guard let term = File(path: path)
			.map(Structure.init)
			.map({ $0.dictionary })
			.map(toAnyObject)
			.flatMap({ JSON(object: $0).map { Term(path: path, JSON: $0) } }) else { return nil }
		self = term
	}
}

if let a = arguments[1].map(Term.init), b = arguments[2].map(Term.init) {
	print(a)
	print(b)
}
