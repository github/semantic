extension StringLiteralConvertible {
	public init(unicodeScalarLiteral: Self.StringLiteralType) {
		self.init(stringLiteral: unicodeScalarLiteral)
	}

	public init(extendedGraphemeClusterLiteral: Self.StringLiteralType) {
		self.init(stringLiteral: extendedGraphemeClusterLiteral)
	}
}
