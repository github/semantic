function Dictionary(object) {
	this.values = {};
	for (key in object) {
		this.values[key] = object[key];
	}
	return this;
}

// forall a b. Dictionary String a -> (a -> b) -> Dictionary String b
Dictionary.prototype.map = function(transform) {
	var copy = new Dictionary();
	for (key in this.values) {
		copy.values[key] = transform(this.values[key], key);
	}
	return copy;
}
