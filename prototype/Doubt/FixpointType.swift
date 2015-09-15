protocol FixpointType {
	typealias Algebra: AlgebraicType
	init(_ algebra: Algebra)
	var out: Algebra { get }
}

protocol AlgebraicType {
	typealias Recur
}

extension FixpointType {
	static func out(fix: Self) -> Algebra {
		return fix.out
	}
}
