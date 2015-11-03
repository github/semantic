function termFromJSON(json) {
	return new Term({
		extract: json.extract,
		unwrap: new Syntax(json.unwrap, function(x) {
			return termFromJSON(x);
		})
	});
}

function Term(object) {
	this.extract = object.extract;
	this.unwrap = object.unwrap;
	return this;
}
