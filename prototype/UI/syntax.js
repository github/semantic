function Syntax(json, continuation) {
	if (json.indexed != null) {
		this.indexed = json.indexed.map(continuation);
	}
	if (json.fixed != null) {
		this.fixed = json.fixed.map(continuation);
	}
	if (json.keyed != null) {
		this.keyed = (new Dictionary(json.keyed)).map(continuation);
	}
	if (json.leaf != null) {
		this.leaf = json.leaf;
	}
	return this;
}

// forall a b. Syntax a -> (a -> b) -> Syntax b
Syntax.prototype.map = function(transform) {
	if (this.leaf != null) {
		return new Syntax({
			leaf: this.leaf
		}, transform);
	}
	if (this.indexed != null) {
		return new Syntax({
			indexed: this.indexed
		}, transform);
	}
	if (this.fixed != null) {
		return new Syntax({
			fixed: this.fixed
		}, transform);
	}
	if (this.keyed != null) {
		return new Syntax({
			keyed: this.keyed.values
		}, transform);
	}
}
