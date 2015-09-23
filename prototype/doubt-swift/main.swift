import Doubt
import SourceKittenFramework

let arguments = BoundsCheckedArray(array: Process.arguments)

extension Term {
	init?(path: String) {
		guard path != "/dev/null" else {
			self = .Empty
			return
		}
		guard let term = File(path: path)
			.map(Structure.init)
			.map({ $0.dictionary })
			.map(toAnyObject)
			.flatMap({ JSON(object: $0).map { Term(path: path, JSON: $0) } }) else { return nil }
		self = term
	}
}

if let a = arguments[1].flatMap(Term.init), b = arguments[2].flatMap(Term.init) {
	print(Diff(a, b))
}
